module CmdHandle (contextHandleCmd,contextHandleLine,handleLine)where

import Types

contextHandleCmd  :: Context -> Command -> IO CmdReturn
contextHandleLine :: Context -> String  -> IO CmdReturn
handleLine :: String -> IO CmdReturn
