module UI
  ( UIState (MkUIState),
    Name,
    PortStatus (..),
    attributes,
    draw,
    getSelected,
    mkUIState,
    onErrorReceived,
    onMessageReceived,
    onPortConnected,
    onPortDisconnected,
    onLeft,
    onRight,
    onUp,
    onDown,
  )
where

import qualified Brick.AttrMap as Attr
import qualified Brick.Types as Types
import qualified Brick.Util as Util
import qualified Brick.Widgets.Border as Border
import qualified Brick.Widgets.Core as Core
import qualified Cursor
import qualified Data.Map.Strict as Map
import qualified Graphics.Vty as Vty
import qualified Settings
import Types (Host, Port)

type Name = ()

type Connections = Map.Map Port Host

data Message = InfoMessage String | ErrorMessage String deriving (Show)

data UIState = MkUIState
  { uiStateHosts :: Cursor.Cursor Host,
    uiStatePorts :: Cursor.Cursor Port,
    uiStateConnections :: Connections,
    uiStateMessages :: [Message]
  }

mkUIState :: Settings.Settings -> UIState
mkUIState settings =
  MkUIState
    { uiStateHosts = Cursor.makeCursor (Settings.hosts settings),
      uiStatePorts = Cursor.makeCursor (Settings.ports settings),
      uiStateConnections = Map.empty,
      uiStateMessages = []
    }

draw :: UIState -> [Types.Widget Name]
draw s = [Core.vBox [hosts, Border.hBorder, messages]]
  where
    hosts = Core.hBox . Cursor.mapWith (drawHost s) . uiStateHosts $ s
    messages = Core.padLeftRight 1 . Core.vBox . map drawMessage . uiStateMessages $ s

drawHost :: UIState -> Bool -> Host -> Types.Widget Name
drawHost s selected host = Core.padLeftRight 1 . Border.border . Core.hLimit 16 . Core.vBox $ box
  where
    box = title : Border.hBorder : Cursor.mapWith drawPort' (uiStatePorts s)
    title = Core.withAttr attr . Core.str $ host
    attr = if selected then aHost <> aSelected else aHost
    drawPort' current = drawPort s host (current && selected)

drawPort :: UIState -> Host -> Bool -> Port -> Types.Widget Name
drawPort s host selected port = Core.padLeft (Core.Pad 2) . Core.withAttr attr . Core.str $ prefix ++ show port
  where
    prefix = if selected then "> " else "  "
    attr = if selected then status <> aSelected else status
    status = case queryPort (uiStateConnections s) host port of
      Available -> aAvailable
      Connected -> aConnected
      InUse -> aInUse

drawMessage :: Message -> Types.Widget Name
drawMessage (InfoMessage message) = Core.withAttr aInfo $ Core.str message
drawMessage (ErrorMessage message) = Core.withAttr aError $ Core.str message

queryPort :: Connections -> Host -> Port -> PortStatus
queryPort connections host port = case Map.lookup port connections of
  Nothing -> Available
  Just usedBy -> if usedBy == host then Connected else InUse

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

aInfo :: Attr.AttrName
aInfo = Attr.attrName "info"

aError :: Attr.AttrName
aError = Attr.attrName "error"

attributes :: Attr.AttrMap
attributes =
  Attr.attrMap
    Vty.defAttr
    [ (aHost, Util.fg Vty.white),
      (aHost <> aSelected, Util.fg Vty.brightCyan `Vty.withStyle` Vty.bold),
      (aAvailable, Util.fg Vty.white),
      (aAvailable <> aSelected, Util.style Vty.bold),
      (aConnected, Util.fg Vty.green),
      (aConnected <> aSelected, Util.style Vty.bold),
      (aInUse, Util.fg Vty.red),
      (aInUse <> aSelected, Util.style Vty.bold),
      (aInfo, Util.fg Vty.white),
      (aError, Util.fg Vty.brightRed)
    ]

onPortConnected :: Port -> Host -> UIState -> UIState
onPortConnected port host s = s {uiStateConnections = Map.insert port host (uiStateConnections s)}

onPortDisconnected :: Port -> UIState -> UIState
onPortDisconnected port s = s {uiStateConnections = Map.delete port (uiStateConnections s)}

onMessageReceived :: Port -> Host -> String -> UIState -> UIState
onMessageReceived port host = appendMessage . InfoMessage . formatMessage host port

onErrorReceived :: Port -> Host -> String -> UIState -> UIState
onErrorReceived port host = appendMessage . ErrorMessage . formatMessage host port

appendMessage :: Message -> UIState -> UIState
appendMessage message s = s {uiStateMessages = take 8 $ message : uiStateMessages s}

formatMessage :: Host -> Port -> String -> String
formatMessage host port message = host ++ ":" ++ show port ++ " '" ++ message ++ "'"

data PortStatus = Available | InUse | Connected

getSelected :: UIState -> Maybe (Host, Port, PortStatus)
getSelected s = case (maybeHost, maybePort) of
  (Just host, Just port) -> Just (host, port, status host port)
  _ -> Nothing
  where
    status = queryPort . uiStateConnections $ s
    maybeHost = Cursor.selected . uiStateHosts $ s
    maybePort = Cursor.selected . uiStatePorts $ s

onLeft :: UIState -> UIState
onLeft s = s {uiStateHosts = Cursor.previous (uiStateHosts s)}

onRight :: UIState -> UIState
onRight s = s {uiStateHosts = Cursor.next (uiStateHosts s)}

onUp :: UIState -> UIState
onUp s = s {uiStatePorts = Cursor.previous (uiStatePorts s)}

onDown :: UIState -> UIState
onDown s = s {uiStatePorts = Cursor.next (uiStatePorts s)}
