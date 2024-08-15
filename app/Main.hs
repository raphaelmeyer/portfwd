module Main where

import qualified Brick.AttrMap as Attr
import qualified Brick.Main as Brick
import qualified Brick.Types as Types
import qualified Brick.Util as Util
import qualified Brick.Widgets.Border as Border
import qualified Brick.Widgets.Core as Core
import qualified Control.Exception as Ex
import qualified Control.Monad as M (void)
import qualified Cursor
import qualified Data.Map.Strict as Map
import qualified Graphics.Vty as Vty
import qualified Settings

type Host = String

type Port = Int

data ConnectionState = ConnectionState
  { sHosts :: Cursor.Cursor Host,
    sPorts :: [Port],
    sConnections :: Map.Map Port Host
  }

type Name = ()

main :: IO ()
main = Ex.handle onError $ do
  result <- Settings.load
  case result of
    Right settings -> M.void $ Brick.defaultMain app (initialState settings)
    Left err -> showError err

onError :: Ex.IOException -> IO ()
onError = showError

showError :: (Show s) => s -> IO ()
showError err = putStrLn $ "❗ " ++ show err

initialState :: Settings.Settings -> ConnectionState
initialState settings =
  ConnectionState
    { sHosts = Cursor.makeCursor (Settings.hosts settings),
      sPorts = Settings.ports settings,
      sConnections = Map.empty
    }

app :: Brick.App ConnectionState e Name
app =
  Brick.App
    { Brick.appDraw = drawUI,
      Brick.appChooseCursor = Brick.neverShowCursor,
      Brick.appHandleEvent = handleEvent,
      Brick.appStartEvent = pure (),
      Brick.appAttrMap = const attributes
    }

drawUI :: ConnectionState -> [Types.Widget Name]
drawUI s = [Core.hBox . Cursor.mapWith (drawHost (sPorts s)) . sHosts $ s]

drawHost :: [Port] -> Bool -> Host -> Types.Widget Name
drawHost ports selected host = if selected then Core.withAttr (Attr.attrName "selectedHost") box else box
  where
    box = Core.padLeftRight 1 . Border.border . Core.hLimit 16 . Core.vBox $ Core.str host : Border.hBorder : map drawPort ports

drawPort :: Int -> Types.Widget Name
drawPort = Core.padLeft (Core.Pad 2) . Core.str . show

attributes :: Attr.AttrMap
attributes =
  Attr.attrMap
    Vty.defAttr
    [ (Attr.attrName "selectedHost", Util.fg Vty.cyan)
    ]

handleEvent :: Types.BrickEvent Name e -> Types.EventM Name ConnectionState ()
handleEvent (Types.VtyEvent ev) = case ev of
  Vty.EvKey Vty.KEsc [] -> Brick.halt
  Vty.EvKey (Vty.KChar 'q') [] -> Brick.halt
  Vty.EvKey Vty.KRight [] -> do
    s <- Types.get
    let s' = s {sHosts = Cursor.next (sHosts s)}
    Types.put s'
  Vty.EvKey Vty.KLeft [] -> do
    s <- Types.get
    let s' = s {sHosts = Cursor.previous (sHosts s)}
    Types.put s'
  _ -> pure ()
handleEvent _ = pure ()
