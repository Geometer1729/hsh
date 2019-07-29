module Types where

import Data.Default
import System.Process
import Data.Semigroup()
import System.IO

debug :: Bool
debug = False
-- not really a type but it's convenient if most things import this

data WaitPlan = Dont | Do | PassHandles

data CmdReturn = CmdReturn {
   shellExit :: Bool
  ,succes    :: Bool
  ,awaits    :: [ProcessHandle]
} 

instance Default CmdReturn where
  def = CmdReturn False True []

instance Semigroup CmdReturn where
  a <> b = CmdReturn (shellExit a || shellExit b) (succes a && succes b) (awaits a ++ awaits b)

instance Monoid CmdReturn where
  mempty = def

data Context = Context {
   wait  :: WaitPlan
  ,stin  :: Maybe Handle 
  ,stout :: Maybe Handle
  ,sterr :: Maybe Handle
}

data Line = Extract String Command | Let [String] [String] | Plain Command deriving (Eq,Ord,Show)
data  Command = Background Command 
              | ITE Command Command Command 
              | Or Command Command 
              | And Command Command 
              | Seq Command Command 
              | Pipe Bool Bool Command Command 
              | Exec String [String] 
              | Infix Command String Command
              deriving(Eq,Ord,Show)

instance Default Context where
  def = Context Do Nothing Nothing Nothing
