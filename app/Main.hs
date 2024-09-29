module Main where

import qualified Application
import qualified Control.Exception as Ex
import qualified Settings

main :: IO ()
main = Ex.handle onError $ do
  result <- Settings.load
  case result of
    Left err -> showError err
    Right settings -> do
      Application.runApplication settings

onError :: Ex.IOException -> IO ()
onError = showError

showError :: (Show s) => s -> IO ()
showError err = putStrLn $ "❗ " ++ show err
