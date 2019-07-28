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

Needs
	* file globing
  * " and \\ support to allow literal strings arguments with spaces etc
  * Pipe stdout stderr seperately
  * common prelude functions
    * flip
      * preluderc
    * forever
      * better pattern matching on arguments and then also prelude rc
	* infix function support
  * Call to haskell
  * implement extract 
  * variable expansion
  * command expansion ie $( echo hi ) in bash
  * better error messages
  * Configurable Prompt ideally parsed from Env variable
	* fancy junk so cd .. works with simlinks 

Installation
  * git clone https://github.com/Geometer1729/hsh.git
	* cd hsh
	* make 
	If anything in there doesn't work please let me know.

Bugs
  * Probably a bunch I don't know about, if you find one please report it
