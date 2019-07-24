module CmdHandle (contextHandleCmd,contextHandleLine) where

import Types
import Control.Monad
import Data.Maybe
import Parse
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Posix.Directory
import System.Posix.User
import System.Process
import Data.Default
import Data.Function
import SubUtils

contextHandleCmd  :: Context -> Command -> IO CmdReturn
contextHandleLine :: Context -> String  -> IO CmdReturn
