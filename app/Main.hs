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
    sPorts :: Cursor.Cursor Port,
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
      sPorts = Cursor.makeCursor (Settings.ports settings),
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

drawHost :: Cursor.Cursor Port -> Bool -> Host -> Types.Widget Name
drawHost ports selected host = Core.padLeftRight 1 . Border.border . Core.hLimit 16 . Core.vBox $ box
  where
    box = title : Border.hBorder : Cursor.mapWith (drawPort selected) ports
    title =
      if selected
        then Core.withAttr (Attr.attrName "selectedHost") $ Core.str host
        else Core.str host

drawPort :: Bool -> Bool -> Port -> Types.Widget Name
drawPort selectedHost selected =
  if selected && selectedHost
    then Core.padLeft (Core.Pad 2) . Core.withAttr (Attr.attrName "selectedPort") . Core.str . show
    else Core.padLeft (Core.Pad 2) . Core.str . show

attributes :: Attr.AttrMap
attributes =
  Attr.attrMap
    Vty.defAttr
    [ (Attr.attrName "selectedHost", Util.fg Vty.blue `Vty.withStyle` Vty.bold),
      (Attr.attrName "selectedPort", Util.fg Vty.green)
    ]

handleEvent :: Types.BrickEvent Name e -> Types.EventM Name ConnectionState ()
handleEvent (Types.VtyEvent ev) = case ev of
  Vty.EvKey Vty.KEsc [] -> Brick.halt
  Vty.EvKey (Vty.KChar 'q') [] -> Brick.halt
  Vty.EvKey Vty.KLeft [] -> do
    Types.modify (\s -> s {sHosts = Cursor.previous (sHosts s)})
  Vty.EvKey Vty.KRight [] -> do
    Types.modify (\s -> s {sHosts = Cursor.next (sHosts s)})
  Vty.EvKey Vty.KUp [] -> do
    Types.modify (\s -> s {sPorts = Cursor.previous (sPorts s)})
  Vty.EvKey Vty.KDown [] -> do
    Types.modify (\s -> s {sPorts = Cursor.next (sPorts s)})
  _ -> pure ()
handleEvent _ = pure ()
