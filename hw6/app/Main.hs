module Main
  ( main
  ) where

import           Control.Monad.Cont            (lift)
import           Data.Maybe                    (fromMaybe)
import           Data.Void                     (Void)
import           Prettyprinter                 (Doc, annotate, pretty, (<+>))
import           Prettyprinter.Render.Terminal (AnsiStyle, Color (Red), color,
                                                putDoc)
import           System.Console.Haskeline
import           System.IO
import           T3.Parser
import           T3.Simulation
import           Text.Megaparsec.Error         (ParseErrorBundle,
                                                errorBundlePretty)

main :: IO ()
main = runInputT defaultSettings loop

loop :: InputT IO ()
loop = do
  minput <- getInputLine "pandemy> "
  case minput of
    Nothing -> return ()
    Just "exit" -> return ()
    Just input -> do
      runOrPrintError (parseProgComand input) runSimulation
      loop

runSimulation :: ProgramOpts -> InputT IO ()
runSimulation programParams = do
  lift
    $ putStrLn
        "Input config parameters: probability, incubationPeriod, illnessDuration, immunityDuration"
        >> hFlush stdout
  confInput <- getInputLine "pandemy> "
  runOrPrintError
    (parseConfig $ fromMaybe "" confInput)
    (\conf -> do
       let days = fromMaybe 15 (limit programParams)
       let prettyGrids =
             prettyComonad19 (fromMaybe 5 (fieldSize programParams))
               <$> take days (simulate conf (fromMaybe 42 (seed programParams)))
       let titles = pretty . (++ ":\n") . ("  Day " ++) . show <$> [1 .. days]
       lift
         (putDoc
            (mconcat ((<> pretty "\n\n") <$> zipWith (<>) titles prettyGrids))))

runOrPrintError ::
     Either (ParseErrorBundle String Void) t
  -> (t -> InputT IO ())
  -> InputT IO ()
runOrPrintError eith action =
  fromRight eith action (lift . printError . errorBundlePretty)

fromRight :: Either a b -> (b -> t) -> (a -> t) -> t
fromRight eith onRight onLeft =
  case eith of
    Left e  -> onLeft e
    Right v -> onRight v

printError :: String -> IO ()
printError err = putDoc (prettyError "Error:" <+> prettyError err)

prettyError :: String -> Doc AnsiStyle
prettyError e = (annotate $ color Red) (pretty e)
