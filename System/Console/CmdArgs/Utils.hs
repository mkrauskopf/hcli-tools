module System.Console.CmdArgs.Utils
    ( cmdArgsOrHelp
    )
  where

import System.Console.CmdArgs.Implicit(Data, cmdArgs)
import System.Environment(getArgs, withArgs)

-- | If no arguments are passed to a program, displays help message and exits,
-- returns parsed arguments otherwise.
cmdArgsOrHelp :: Data a => a -> IO a
cmdArgsOrHelp opts = do
  mainArgs <- getArgs
  (if null mainArgs then withArgs ["-?"] else id) (cmdArgs opts)

