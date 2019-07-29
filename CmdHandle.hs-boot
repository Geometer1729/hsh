module CmdHandle (contextHandleCmd,contextHandleLine,handleLine,contextHandleLineData)where

import Types

contextHandleCmd  :: Context -> Command -> IO CmdReturn
contextHandleLine :: Context -> String  -> IO CmdReturn
contextHandleLineData :: Context -> Line  -> IO CmdReturn
handleLine :: String -> IO CmdReturn
