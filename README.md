# hsh
A shell written in haskell, with the hope of capturing some of the succinctness and expressiveness of haskell.

Has
  * Basic commands with arguments
  * Pipeing with >>=
  * Sequencing with >>
  * Background with &
  * logic 
    * if then else 
    * && and ||
  * Built-ins
    * cd 
    * print var 
    * lineMap
    * True
    * False
  * let
    * supports lambda functions and function definitions
  * execute by absolute filePath or path from current working directory
	* Ctrl-c Ctrl-d support
	* file globing
  * " and \\ support to allow literal strings arguments with spaces etc
	* backticks allow evaluating commands inside commands
  * store output as var with <-, ie x <- ls

Needs
  * Pipe stdout stderr seperately
  * variable expansion
  * better error messages
	* fancy junk so cd .. works with simlinks 
	* infix function support
  * common prelude functions
    * flip
      * preluderc
    * forever
      * better pattern matching on arguments and then also prelude rc
  * Call to haskell
  * Configurable Prompt ideally parsed from Env variable

Installation

git clone https://github.com/Geometer1729/hsh.git

cd hsh

make 

If anything in there doesn't work please let me know.

Bugs
  * Probably a bunch I don't know about, if you find one please report it
