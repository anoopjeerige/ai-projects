;;; Project	: 	GP Arithmetic Critters
;;; Filename	:	CPSC-481-p2b_DASA.lisp
;;; Class	: 	CPSC - 481 13593 AI
;;; Section 	: 	3
;;; Team	: 	DASA
;;; Authors	: 	Devon Cook
;;; 	 		Anoop Jeerige
;;;             	Sean Holmes
;;;			Andrew Nguyen
;;; Contact 	: 	dcook03@csu.fullerton.edu
;;; 	       		anoopjeerige@csu.fullerton.edu
;;;			Seanholmes@csu.fullerton.edu
;;;			andrew_dd_n@csu.fullerton.edu
;;;
;;; References
;;; 1) http://aerique.blogspot.com/2011/01/baby-steps-into-genetic-programming.html
;;; 2) http://cswww.essex.ac.uk/staff/rpoli/gp-field-guide/
;;; 3) http://clhs.lisp.se/Front/X_Symbol.htm 
;;; 
;;; LISP program to generate an arithmetic expression using Genetic Programming (GP),
;;; that can produce an ouptut matching a given target and given inputs x y z from the sample data.
;;;
;;; GP Parameters
;;; Function Set : {+, -, *, div1} NB. div1 is safe division to handle divide by zero 
;;; Terminal Set : {x, y, z, -9, -8 ... 0 ... 8, 9 }
;;; Fitness : Absolute difference between the output and actual target ranging between [1 ... 0]
;;; Population : n=100
;;; Genetic Operations : Cross-Over and Mutation
;;; Goal : Arithmetic expression that evaluates to the given target value for the input values of x y z
;;;
;;; Running GP
;;; 1. Create initial population
;;; 	(defparameter population (create-initial-population *operators* *variables* n))
;;; 2. Evaluate the fitness of the population, if desired fitness is not met then advance m times the generation of the population
;;; 	(loop repeat m for i from 0 do (format t "[~S] " i) (setf population (advance-generation population target *operators* *variables* x y z)))
;;; 3. If desired fitness is achieved, then get the experssion with that fitness 
;;; 	(defparameter best-form (first (evaluate-population population target x y z)))
;;; 4. Run the expression with the inputs to verify the output
;;;		(run-form (getf best-form :form) x y z)

;; Parameters defined to represent operators and input operands
(defparameter *operators* '(+ - * div1))
(defparameter *variables* '(=x= =y= =z=))

;; Function to emulate safe division, thus checking divide by zero case
(defun div1 (x y &optional (z 1)) ;; handle expressions with arity upto three
  (if (or (eq y 0) (eq z 0)) 1 (/ x y z))) ;; return 1 if divide by zero case else the actual division

;; Function to select random element from a given sequence
(defun random-elt (sequence)
  (let ((seq-length (length sequence))) ;; length of the sequence
    (when (> seq-length 0) ;; if sequence atleast one element
      (elt sequence (random seq-length))))) ;; pick random element from sequence

;; Function to generate random expression forms using the *operators* and *variables*
(defun random-form (operators variables &optional (max-depth 4));; depth bound to control the growth size of the expression tree
  (append (list (random-elt operators)) ;; pick random operator from the *operators* set [+ - * div1]
          (loop repeat 3  ; arity to produce quadratic expressions
                collect (let ((random-nr (random 100)));; random number to contorl probability of the expression formation
                          (if (> max-depth 0)
                              (cond ((< random-nr 50) ;; 50% chance to call itself to grow the expression
                                     (random-form operators variables (- max-depth 1))) ;; reduce depth to control expression growth
                                    ((< random-nr 75) (random 10)) ;; 25% chance to add constant operand from a range [0 .. 9]
                                    (t                (random-elt variables))) ;; 25% chance to add variable operand from the *variables* set [=x= =y= =z=]
                              (cond ((< random-nr 50) (- 0 (random 10))) ;; 50% chance to add constant operand from a range [-9 .. 0]
                                    (t                (random-elt variables))))))))  ;; 50% chance to add variable operand from the *variables* set [=x= =y= =z=]

;; Function to run the generated random expression form with the given inputs of x y z
(defun run-form (form x y z)
  (handler-case (funcall (eval `(lambda (=x= =y= =z=) ,form)) ;; error handler for forms that evaluate as invalid 
                           x y z)
	(error () nil)))

;; Function to calculate the fitness of a given expression form, its target value and the input values of x y z
(defun fitness (form target x y z)
  (let ((output (run-form form x y z))) ;; evaluate the actual output of the expression
  (let ((difference (when output (abs (- target output))))) ;; calculate the absolute difference between the output and target values
  (when output (/ 1.0 (+ 1 difference)))))) ;; add one to this difference and divide 1 by this difference


;; Function to count the number of nodes in the given expression form
(defun n-nodes (form)
  (let ((nodes 1)) ;; initialize count
    (labels ((traverse-nodes (subform) ;; define a local function to traverse the expression tree
				(loop for node in subform ;; loop through the nodes of the expression tree
					  do (incf nodes) ;; increment count
						 (when (listp node) ;; call the local function recursively when node is a list
							(traverse-nodes node)))))
	  (traverse-nodes form)) ;; call local function
	nodes)) ;; return node count

;; Function to select random node from a given expression form
(defun random-node (form)
	(let* ((index 1) ;; index to the nodes of the expression tree
		   (nodes-1 (- (n-nodes form) 1)) ;; get the node count, one less than the actual count
		   (random-node-index (+ (random nodes-1) 1))) ;; select a random number within the node count as the random node index
	  (labels ((traverse-nodes (subform) ;; define a local function to traverse the expression tree
				  (loop for node in subform ;; loop through the nodes of the expression tree
						do (when (= index random-node-index) ;; when the index is same as the earlier selected random node index return that node and its index as a list
							  (return-from random-node
											(list :index index :node node))) ;; form the return list as property list :index => index and :node => node
						   (incf index) ;; increment the index count
						   (when (listp node) ;; call the local function recursively when node is a list
							  (traverse-nodes node)))))
		(traverse-nodes form))))

;; Function to replace a node for a given expession form given the index and new node for replacement
;; Traverse the expression tree and form the tree list again with the expection of having the given new node at the given index
(defun replace-node (form node-index new-node)
  (let ((index 0)) ;; index to the nodes of the expression tree
	(labels ((traverse-nodes (subform) ;; define a local function to traverse the expression tree
				(loop for node in subform ;; loop through the nodes of the expression tree
					  do (incf index) ;; increment the index count
					  when (= index node-index) ;; traverse to the node in the tree for the given index and collect the new node into the tree list instead of the old node 
						collect new-node
					  when (and (/= index node-index) ;; if node is not the selected index and is not a list, then add the node to the tree list
								(not (listp node)))
						collect node
					  when (and (/= index node-index) ;; if node is not the selected index but is a list, then call local function recursively to traverse that node list
								(listp node))
						collect (traverse-nodes node))))
	  (traverse-nodes form))))

;; Function to perform the genetic operation of cross-over
;; For the given two expressions, the function takes a random node index from the first expression and replaces that index with a random node from the second expression
(defun cross-over (form1 form2)
  (let ((rnode1 (random-node form1)) ;; select a random node from the first expression
		(rnode2 (random-node form2))) ;; select a random node from the second expression
	(replace-node form1 (getf rnode1 :index) (getf rnode2 :node)))) ;; get the index and the node for the random nodes using the property lists and perform the replacement

;; Function to perform the genetic operation of mutation
;; For a given expression, the function takes a random node index and replaces the node at that index with a new generated random expression
(defun mutate (form operators variables)
  (let ((rform (random-form operators variables)) ;; generate a new random expression using the *operators* and *variables*
		(rnode (random-node form))) ;; select a random node from the expression
	(replace-node form (getf rnode :index) rform))) ;; get the index for the random node using the property list and perform the replacement


;; Function to create the intial population list of random expressions
(defun create-initial-population (operators variables &optional (size 100))
  (loop repeat size
        collect (random-form operators variables))) ;; generate a new random expression using the *operators* and *variables* sets and collect them into a list

;; Function to evaluate the fitness of each of the random expression in the given population, for the given target value and the input values of x y z
(defun evaluate-population (population target x y z)
  (loop for form in population ;; loop to evaluate each expression in the given population
		for fitness = (fitness form target x y z) ;; evaluate the expression's fitness for the given target and input values
		when fitness collect (list :fitness fitness :form form) into result ;; build a property list of :fitness => fitness and :form => random expression
		finally (return (sort result ;; sort the property list according to fitness with higher value first
							  (lambda (a b)
								(> (getf a :fitness) (getf b :fitness)))))))

;; Function to return the limit elements for a given sequence
(defun head (sequence &optional (limit 1))
  (if (<= limit 0)
	  nil
	  (if (< (length sequence) limit) ;; if given sequence is smaller than limit, 
		  sequence ;; then return sequence
		  (subseq sequence 0 limit)))) ;; else return limit elements from sequence

;; Function to return the average fitness of the given evaluated population
;; The evluated population is a property list constructed from the population list of epxression and has :fitness => fitness :form => expression 
(defun avg-fitness (population)
	(loop for form in population ;; loop through the evlauted population list
		for avg = (getf form :fitness) ;; get the fitness for the evaluated expression
		sum avg into total ;; calculate the average
	finally 
		(return (/ total (length population)))))

;; Function to advance the generation for the given population
;; For a given population list, the target and inputs the function evaluates the fitness of the population, and
;; until the desired fitness is reached, the genetic operations of cross-over and mutations are performed on the current population,
;; thus producing the next generation for futher repeations
(defun advance-generation (population target operators variables x y z
                           &optional (max-population 100))
  (let ((epop (evaluate-population population target x y z)));; evaluate the given population, for the given target value and the input values of x y z 
    (format t "Current population statistics Best fitness: ~S Average fitness: ~S Worst fitness: ~S~%" ;; display the best fitness of the population
            (getf (first epop) :fitness) ;; get the best fitness of the evaluated population
			(avg-fitness epop) ;; get the average fitness of the evaluated population
			(getf (nth (- (length epop) 1) epop) :fitness)) ;; get the worst fitness of the evaluated population
    (loop for plist in (head epop max-population) ;; loop to perform genetic operations on the current population
          for i from 0 ;; generation count for display
          for fitness = (getf plist :fitness) ;; select fitness of the expression
          for form = (getf plist :form) ;; select the expression
          collect form
          when (<= (random 1.0) fitness) ;; until desired fitness
            collect (if (<= (random 100) 90)
                        (cross-over form (getf (random-elt epop) :form)) ;; 90% chance to perform cross-over of the selected expression with a random expression
                        (mutate form operators variables)) ;; 10% chance to perform mutation of the selected expressionS
          when (<= (random 100) 2) collect (random-form operators variables)))) ;; 2% change to add a new random expression to the population
