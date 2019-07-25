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

Needs
  * common prelude functions
    * flip
    * forever
    * ++
    * arithmetic
  * Call to haskell
  * implement extract and forward
  * Configurable Prompt ideally parsed from Env variable
  * ~ expansion in more contexts
  * variable expansion
  * command expansion ie $( echo hi ) in bash
  * better error messages
  * execute by absolute filePath or path from current working directory

Installation
  Several Cabal Packages are required to compile
    * readline
    * maybe 1 or 2 others I forget which
  Then just run make.
    If there are erros it probably means you don't have a cabal package I forgot I installed.
    The error should look like import ______ no such something, typically this can be resolved by searching the import in hackage or hoogle and installing the cabal package.
    Pleas tell me what the error is and if you fix it what package you needed to fix it was. 
  if runing hsh works and you are brave enough chsh -s /bin/hsh will make it your default shell.
  It will complain if you don't add /bin/hsh to the end of /etc/shells . I'm not sure if this matters.

Bugs
  * Extract Not implemented
  * Probably a bunch more I don't know about, if you find one please report it
