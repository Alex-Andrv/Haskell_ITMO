module Main
  ( main,
  )
where

import HW3.Base ()
import HW3.Evaluator
import HW3.Parser
import HW3.Pretty
import HW3.Action
import System.Console.Haskeline
import Data.Set (Set, member, fromList)
import Control.Monad.Cont (lift)

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "hi, Lasciate ogni speranza, voi ch'entrate> "
      case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just input -> do
          case parse input of
            Right expr -> do
              ans <- lift $ runHIO (eval expr) (fromList [AllowRead, AllowWrite])
              case ans of
                Left err -> outputStrLn $ (show err)
                Right val -> outputStrLn $ (show (prettyValue val))
            Left err -> outputStrLn $ show err
          loop