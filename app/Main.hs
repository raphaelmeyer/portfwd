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

type Connections = Map.Map Port Host

data ConnectionState = ConnectionState
  { sHosts :: Cursor.Cursor Host,
    sPorts :: Cursor.Cursor Port,
    sConnections :: Connections
  }

data PortStatus = Available | InUse | Connected

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
drawUI s = [Core.hBox . Cursor.mapWith (drawHost s) . sHosts $ s]

drawHost :: ConnectionState -> Bool -> Host -> Types.Widget Name
drawHost s selected host = Core.padLeftRight 1 . Border.border . Core.hLimit 16 . Core.vBox $ box
  where
    box = title : Border.hBorder : Cursor.mapWith drawPort' (sPorts s)
    title = Core.withAttr attr . Core.str $ host
    attr = if selected then aHost <> aSelected else aHost
    drawPort' current = drawPort s host (current && selected)

drawPort :: ConnectionState -> Host -> Bool -> Port -> Types.Widget Name
drawPort s host selected port = Core.padLeft (Core.Pad 2) . Core.withAttr attr . Core.str . show $ port
  where
    attr = if selected then status <> aSelected else status
    status = case queryPort (sConnections s) host port of
      Available -> aAvailable
      Connected -> aConnected
      InUse -> aInUse

aHost :: Attr.AttrName
aHost = Attr.attrName "host"

aAvailable :: Attr.AttrName
aAvailable = Attr.attrName "available"

aInUse :: Attr.AttrName
aInUse = Attr.attrName "in-use"

aConnected :: Attr.AttrName
aConnected = Attr.attrName "connected"

aSelected :: Attr.AttrName
aSelected = Attr.attrName "selected"

attributes :: Attr.AttrMap
attributes =
  Attr.attrMap
    Vty.defAttr
    [ (aHost, Util.fg Vty.white),
      (aHost <> aSelected, Util.fg Vty.blue `Vty.withStyle` Vty.bold),
      (aAvailable, Util.fg Vty.white),
      (aAvailable <> aSelected, Util.style Vty.bold),
      (aConnected, Util.fg Vty.green),
      (aConnected <> aSelected, Util.style Vty.bold),
      (aInUse, Util.fg Vty.red),
      (aInUse <> aSelected, Util.style Vty.bold)
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
  Vty.EvKey Vty.KEnter [] -> do
    Types.modify togglePort
  _ -> pure ()
handleEvent _ = pure ()

togglePort :: ConnectionState -> ConnectionState
togglePort s = case (Cursor.selected . sPorts $ s, Cursor.selected . sHosts $ s) of
  (Just port, Just host) -> toggle port host
  _ -> s
  where
    toggle port host = case queryPort (sConnections s) host port of
      Available -> s {sConnections = connectPort port host (sConnections s)}
      Connected -> s {sConnections = disconnectPort port (sConnections s)}
      _ -> s

queryPort :: Connections -> Host -> Port -> PortStatus
queryPort connections host port = case Map.lookup port connections of
  Nothing -> Available
  Just usedBy -> if usedBy == host then Connected else InUse

connectPort :: Port -> Host -> Connections -> Connections
connectPort = Map.insert

disconnectPort :: Port -> Connections -> Connections
disconnectPort = Map.delete
