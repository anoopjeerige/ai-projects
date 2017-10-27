;;; Project	: 	DFS Pitchers Part 1
;;; Filename	:	dfs-pitchers.lisp
;;; Class	: 	CPSC - 481 13593 AI
;;; Section 	: 	3
;;; Team	: 	DAX
;;; Authors	: 	Devon Cook
;;; 	 		Anoop Jeerige
;;; Contact : 		dcook03@csu.fullerton.edu
;;; 	       		anoopjeerige@csu.fullerton.edu
;;; 
;;; LISP program to find the sequence of to solve the pitcher puzzle using
;;; Depth-First Search (DFS) 
;;;
;;; The recursive function dfs-pitcher-solver that takes three arguments -
;;; start state, goal state and the list of visited states and returns the 
;;; the list of states whose sequence represents the solution
;;; 
;;; Helper functions - make-state and xxxx-pitcher
;;; make-state function creates a state as a list given the contents of the three pitchers
;;; xxxx-pitcher functions the return the content of the respective pitcher called
;;;
;;; The six move functions xxxx-to-xxxx that take agrument current state and 
;;; returns the a new state that represents single move

;; Helper function to create a state given the individual contents of the three pitchers
(defun make-state (p1 p2 p3) (list p1 p2 p3))

;; Helper functions to retrun the contents of a given state for the respective pitcher
(defun first-pitcher (state) (nth 0 state))
(defun second-pitcher (state) (nth 1 state))
(defun third-pitcher (state) (nth 2 state))

;; Move functions that return a state representing the next possible move from a source to target
(defun first-to-second (state)
	(make-state (max 0 (- (+ (first-pitcher state) (second-pitcher state)) 9)) 
				(min (+ (first-pitcher state) (second-pitcher state)) 9) 
				(third-pitcher state)))

(defun first-to-third (state)
	(make-state (max 0 (- (+ (first-pitcher state) (third-pitcher state)) 7)) 
				(second-pitcher state)
				(min (+ (first-pitcher state) (third-pitcher state)) 7)))

(defun second-to-third (state)
	(make-state (first-pitcher state)
				(max 0 (- (+ (second-pitcher state) (third-pitcher state)) 7))
				(min (+ (second-pitcher state) (third-pitcher state)) 7)))

(defun second-to-first (state)
	(make-state (min (+ (second-pitcher state) (first-pitcher state)) 16)
				(max 0 (- (+ (second-pitcher state) (first-pitcher state)) 16)) 
				(third-pitcher state)))

(defun third-to-first (state)
	(make-state (min (+ (third-pitcher state) (first-pitcher state)) 16)
				(second-pitcher state)
				(max 0 (- (+ (third-pitcher state) (first-pitcher state)) 16))))

 (defun third-to-second (state)
	(make-state (first-pitcher state)
				(min (+ (third-pitcher state) (second-pitcher state)) 9)
				(max 0 (- (+ (third-pitcher state) (second-pitcher state)) 9))))

;; The recursive function that searches the space in a depth first fashion
(defun dfs-pitcher-solver (state goal been-list)
	(cond ((> (length been-list) 15) nil) ;Check for a solution with minimal moves
		  ;; Check if state is equal to goal state, 
		  ;; if true add state to visited states 
          ;; and reverse the list for readability
		  ((equal state goal) (reverse (cons state been-list))) 	
		  ;; Check if state is already visited in order to prevent looping
		  ((not (member state been-list :test #'equal))
			   ;; Else call dfs-pictcher passing -
			   ;;                                 1) next move as child state generated from parent state
			   ;;  								  2) goal state
			   ;;                                 3) the visited list with the parent state added
			   (or (dfs-pitcher-solver (first-to-second state) goal (cons state been-list))
					(dfs-pitcher-solver (second-to-third state) goal (cons state been-list))
					(dfs-pitcher-solver (third-to-second state) goal (cons state been-list))
					(dfs-pitcher-solver (third-to-first state) goal (cons state been-list))
					(dfs-pitcher-solver (second-to-first state) goal (cons state been-list))
					(dfs-pitcher-solver (first-to-third state) goal (cons state been-list))))))
