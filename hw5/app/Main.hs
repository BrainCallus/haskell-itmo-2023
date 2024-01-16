{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE TypeApplications #-}

module Main
  ( main
  ) where

import           Control.Arrow                 (left)
import           Control.Exception
import           Control.Monad                 (join)
import           Control.Monad.Cont            (lift)
import           Data.Set
import           HW5.Action
import           HW5.Base                      (HiExpr, HiValue)
import           HW5.Evaluator                 (eval)
import           HW5.Parser
import           HW5.Pretty                    (errorStyle, prettyValue)
import           Prettyprinter                 ((<+>))
import           Prettyprinter.Render.Terminal (putDoc)
import           System.Console.Haskeline
import           Text.Megaparsec               (errorBundlePretty)

main :: IO ()
main = runInputT defaultSettings loop

loop :: InputT IO ()
loop = do
  minput <- getInputLine "hi> "
  case minput of
    Nothing -> return ()
    Just input -> do
      case parse input of
        Left e       -> lift (printError (errorBundlePretty e))
        Right parsed -> doEval parsed
    --Just "exit" -> return ()
  loop

doEval :: HiExpr -> InputT IO ()
doEval parsed = do
  let evaluation = left show <$> runHIO (eval parsed) permissions
  withHandling <- lift $ try @SomeException evaluation
  printResult (join $ left show withHandling)
  lift (putStrLn "")

permissions :: Set HiPermission
permissions = fromList [AllowRead, AllowWrite, AllowTime]

printResult :: Either String HiValue -> InputT IO ()
printResult evaluated =
  case evaluated of
    Left e   -> lift $ printError e
    Right hv -> lift $ putDoc (prettyValue hv)

printError :: String -> IO ()
printError e = putDoc (errorStyle "Error:" <+> errorStyle e)
