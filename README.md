# AI-Projects
## Development Environment Setup
(Windows 10)
LispIDE, a graphical shell for CLISP, as well as an ANSI Common Lisp implementation of Common Lisp can be used as a development and testing environment for the project.

The source links for downloading CLisp and LispIDE are given below:
CLisp - 2.49
https://sourceforge.net/projects/clisp/
LispIDE
https://www.daansystems.com/lispide/

First, install CLisp using the downloaded installer package - clisp-2.49-win32-mingw-big.exe. 
Next, extract contents of LispIDE.zip to any desired location. 
Then, run the LispIDE_Setup.exe installer package to install the IDE. 
Once installed, run LispIDE.exe from the installed location and then select the clisp.exe executable from the previously installed clisp-2.49 folder when prompted for a clisp implementation.

(MAC OSX)
A package manager such as MacPorts or HomeBrew can be used to install Common Lisp on your system.

In the Terminal, enter the command '/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"' without the single quotation marks. After HomeBrew is installed, enter the command 'brew install clisp' without the single quotation marks.

## DFS Pitchers
### Intro
The DFS Pitchers project presents a recursion based solution that searches the state space in depth first manner to solve the pitcher problem. The problem consists of three pitchers with capacities of 16,9 and 7 units, respectively. The starting state has pitcher 1 filled fully with 16 units and the other two are empty. The goal is to distribute the initial 16 units into the first and second pitcher as 8 units each and the third remains empty. Each move only allows pouring from one pitcher to another until either you empty the source or you fill the target pitcher. The solution should provide the sequence of moves that allow one to reach the goal state from the start state.

Common Lisp, a dialect of Lisp programming language, is used for coding the solution.
### Usage
In the LispIDE, use the File->Open option to open the lisp file "dfs-pitchers.lisp". This lisp file contains the solution code. Then in the console area of the LispIDE, present at the bottom, type in the load command as (load "<path>:\\dfs-pitchers.lisp") to load the definitions from the file.

In the Mac Terminal, load Common Lisp with the command 'clisp' without the single quotation marks.

In either case, call the function "dfs-pitcher-solver" passing the three arguments - start state, goal state and list of visited states (initialize to empty) as shown below.
(dfs-pitcher-solver '(16 0 0) '(8 8 0) '()). The solution prints out the list of moves for the pitcher problem.

## GP Arithmetic Critters
### Intro
The GP Arthimetic Critters project presents a Evolutionary Programming based solution to generate an arithmetic expression using Genetic Programming (GP), that can produce an ouptut matching a given target and given inputs x y z from the sample data.

The main reference for the project is the blog - "Baby steps into Genetic Programming" by Aerique, that presents a neat introductory hands-on to GP.
References
1) http://aerique.blogspot.com/2011/01/baby-steps-into-genetic-programming.html
2) http://cswww.essex.ac.uk/staff/rpoli/gp-field-guide/
3) http://clhs.lisp.se/Front/X_Symbol.htm 
 
GP Parameters
Function Set : {+, -, *, div1} NB. div1 is safe division to handle divide by zero 
Terminal Set : {x, y, z, -9, -8 ... 0 ... 8, 9 }
Fitness : Absolute difference between the output and actual target ranging between [1 ... 0]
Population : n=100
Genetic Operations : Cross-Over and Mutation
Objective : Arithmetic expression that evaluates to the given target value for the input values of x y z

Sample Data - Format (x y z target)
(1 0 2 2)
(0 -2 1 -16)
(9 8 -6 72)  
(9 -7 5 113)  
(-8 7 3 150)  
(-4 -5 -3 58)  
(5 4 -5 20)  
(6 -4 6 41)  
(-5 3 -7 -24)  
(-6 -5 9 -18) 

Common Lisp, a dialect of Lisp programming language, is used for coding the solution.

### Usage
In the LispIDE, use the File->Open option to open the lisp file "CPSC-481-p2b_DASA.lisp". This lisp file contains the solution code. Then in the console area of the LispIDE, present at the bottom, type in the load command as (load "<path>:\\CPSC-481-p2b_DASA.lisp") to load the definitions from the file.

In the Mac Terminal, load Common Lisp with the command 'clisp' without the single quotation marks.

Running GP
1. Create initial population
 	(defparameter population (create-initial-population *operators* *variables* n))
2. Evaluate the fitness of the population, if desired fitness is not met then advance m times the generation of the population
 	(loop repeat m for i from 0 do (format t "[~S] " i) (setf population (advance-generation population target *operators* *variables* x y z)))
3. If desired fitness is achieved, then get the experssion with that fitness 
 	(defparameter best-form (first (evaluate-population population target x y z)))
4. Run the expression with the inputs to verify the output
	(run-form (getf best-form :form) x y z)

Example Run for sample data (-6 -5 9 -18)
1. Create initial population of n=100
 	(defparameter population (create-initial-population *operators* *variables* 100))
2. Evaluate the fitness of the population, if desired fitness is not met then advance 10 times the generation of the population
 	(loop repeat 10 for i from 0 do (format t "[~S] " i) (setf population (advance-generation population -18 *operators* *variables* -6 -5 9)))
3. If desired fitness is achieved, then get the experssion with that fitness 
 	(defparameter best-form (first (evaluate-population population -18 -6 -5 9)))
4. Run the expression with the inputs to verify the output
	(run-form (getf best-form :form) -6 -5 9)
  
### Extras
The supplemental file "GP_Arithmetic_Critters_Runs.xlsx" contains the runs for all the sample data. 
It captures best, average and worst fitness across each generation and presents the same across a graph to show the fitness trends.

### Bugs
The arithmetic expressions are formed randomly using the random-form function where all the operators from the function set {+, -, *, div1} have an arity of 3.
Hence this sometimes leads to a polynomial of degree higher than 2. The random-form use a grow algorithm as mentioned in http://cswww.essex.ac.uk/staff/rpoli/gp-field-guide/22InitialisingthePopulation.html, a future workaround would be to use a full method and control the variables picked from the Terminal Set : {x, y, z, -9, -8 ... 0 ... 8, 9 }.


