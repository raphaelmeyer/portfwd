module Settings where

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
