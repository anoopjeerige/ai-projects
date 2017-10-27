# AI-Projects
## Development Environment Setup
(Windows 10)
LispIDE, a graphical shell for CLISP, as well as an ANSI Common Lisp implementation of Common Lisp can be used as a development and testing environment for the project.

The source links for downloading CLisp and LispIDE are given below:
CLisp - 2.49
https://sourceforge.net/projects/clisp/
LispIDE
https://www.daansystems.com/lispide/

First, install CLisp using the downloaded installer package - clisp-2.49-win32-mingw-big.exe. Next, extract contents of LispIDE.zip to any desired location. Then, run the LispIDE_Setup.exe installer package to install the IDE. Once installed, run LispIDE.exe from the installed location and then select the clisp.exe executable from the previously installed clisp-2.49 folder when prompted for a clisp implementation.

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
