module Settings where

import qualified System.Environment as Env
import System.FilePath ((</>))
import qualified System.IO
import qualified Text.JSON as Json

type Host = String

type Port = Int

data Settings = Settings
  { hosts :: [Host],
    ports :: [Port]
  }
  deriving (Eq, Show)

instance Json.JSON Settings where
  readJSON (Json.JSObject obj) = do
    hs <- Json.valFromObj "hosts" obj
    ps <- Json.valFromObj "ports" obj
    pure (Settings hs ps)
  readJSON _ = undefined

  showJSON = undefined

newtype ParseError = ParseError String deriving (Eq, Show)

path :: String -> String
path home = home </> ".config/portfwd/settings.json"

load :: IO (Either ParseError Settings)
load = do
  home <- Env.getEnv "HOME"
  content <- System.IO.readFile (path home)
  pure $ case Json.decode content :: Json.Result Settings of
    Json.Ok settings -> Right settings
    Json.Error err -> Left (ParseError err)
