module Main where

import qualified Brick.AttrMap as Attr
import qualified Brick.BChan as BChan
import qualified Brick.Main as Brick
import qualified Brick.Types as Types
import qualified Brick.Util as Util
import qualified Brick.Widgets.Border as Border
import qualified Brick.Widgets.Core as Core
import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Ex
import qualified Control.Monad as M (void)
import Control.Monad.IO.Class (liftIO)
import qualified Cursor
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Map.Strict as Map
import qualified Graphics.Vty as Vty
import qualified Settings
import qualified System.IO as IO
import qualified System.Process as Proc

type Host = String

type Port = Int

type Connections = Map.Map Port (Host, Proc.ProcessHandle)

data Message = InfoMessage String | ErrorMessage String deriving (Show)

data ConnectionEvent
  = PortConnected (Port, Host, Proc.ProcessHandle)
  | PortDisconnected Port
  | PortMessage (Port, Host, String)
  | PortError (Port, Host, String)

data ConnectionState = ConnectionState
  { sHosts :: Cursor.Cursor Host,
    sPorts :: Cursor.Cursor Port,
    sConnections :: Connections,
    sChan :: BChan.BChan ConnectionEvent,
    sMessages :: [Message]
  }

data PortStatus = Available | InUse | Connected

type Name = ()

main :: IO ()
main = Ex.handle onError $ do
  result <- Settings.load
  case result of
    Left err -> showError err
    Right settings -> do
      chan <- BChan.newBChan 8
      (_, vty) <- Brick.customMainWithDefaultVty (Just chan) app (initialState settings chan)
      Vty.shutdown vty

onError :: Ex.IOException -> IO ()
onError = showError

showError :: (Show s) => s -> IO ()
showError err = putStrLn $ "❗ " ++ show err

initialState :: Settings.Settings -> BChan.BChan ConnectionEvent -> ConnectionState
initialState settings chan =
  ConnectionState
    { sHosts = Cursor.makeCursor (Settings.hosts settings),
      sPorts = Cursor.makeCursor (Settings.ports settings),
      sConnections = Map.empty,
      sChan = chan,
      sMessages = []
    }

app :: Brick.App ConnectionState ConnectionEvent Name
app =
  Brick.App
    { Brick.appDraw = drawUI,
      Brick.appChooseCursor = Brick.neverShowCursor,
      Brick.appHandleEvent = handleEvent,
      Brick.appStartEvent = pure (),
      Brick.appAttrMap = const attributes
    }

drawUI :: ConnectionState -> [Types.Widget Name]
drawUI s = [Core.vBox [hosts, Border.hBorder, messages]]
  where
    hosts = Core.hBox . Cursor.mapWith (drawHost s) . sHosts $ s
    messages = Core.padLeftRight 1 . Core.vBox . map drawMessage . sMessages $ s

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

drawMessage :: Message -> Types.Widget Name
drawMessage (InfoMessage message) = Core.withAttr aInfo $ Core.str message
drawMessage (ErrorMessage message) = Core.withAttr aError $ Core.str message

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

handleEvent :: Types.BrickEvent Name ConnectionEvent -> Types.EventM Name ConnectionState ()
handleEvent (Types.VtyEvent ev) = case ev of
  Vty.EvKey Vty.KEsc [] -> shutdownApp
  Vty.EvKey (Vty.KChar 'q') [] -> shutdownApp
  Vty.EvKey Vty.KLeft [] -> do
    Types.modify (\s -> s {sHosts = Cursor.previous (sHosts s)})
  Vty.EvKey Vty.KRight [] -> do
    Types.modify (\s -> s {sHosts = Cursor.next (sHosts s)})
  Vty.EvKey Vty.KUp [] -> do
    Types.modify (\s -> s {sPorts = Cursor.previous (sPorts s)})
  Vty.EvKey Vty.KDown [] -> do
    Types.modify (\s -> s {sPorts = Cursor.next (sPorts s)})
  Vty.EvKey Vty.KEnter [] -> do
    s <- Types.get
    s' <- liftIO . togglePort $ s
    Types.put s'
  _ -> pure ()
handleEvent (Types.AppEvent (PortConnected (port, host, handle))) =
  Types.modify (\s -> s {sConnections = Map.insert port (host, handle) (sConnections s)})
handleEvent (Types.AppEvent (PortDisconnected port)) =
  Types.modify (\s -> s {sConnections = Map.delete port (sConnections s)})
handleEvent (Types.AppEvent (PortMessage (port, host, message))) =
  Types.modify . appendMessage . InfoMessage $ formatMessage host port message
handleEvent (Types.AppEvent (PortError (port, host, message))) =
  Types.modify . appendMessage . ErrorMessage $ formatMessage host port message
handleEvent _ = pure ()

appendMessage :: Message -> ConnectionState -> ConnectionState
appendMessage message s = s {sMessages = take 8 $ message : sMessages s}

formatMessage :: Host -> Port -> String -> String
formatMessage host port message = host ++ ":" ++ show port ++ " '" ++ message ++ "'"

shutdownApp :: Types.EventM Name ConnectionState ()
shutdownApp = do
  s <- Types.get
  mapM_ (liftIO . (`disconnectPort` s)) (Map.keys . sConnections $ s)
  Brick.halt

togglePort :: ConnectionState -> IO ConnectionState
togglePort s = do
  case getSelected s of
    Just (host, port, status) -> action s
      where
        action = case status of
          Available -> connectPort port host
          Connected -> disconnectPort port
          _ -> pure
    Nothing -> pure s

getSelected :: ConnectionState -> Maybe (Host, Port, PortStatus)
getSelected s = case (Cursor.selected . sHosts $ s, Cursor.selected . sPorts $ s) of
  (Just host, Just port) -> Just (host, port, status host port)
  _ -> Nothing
  where
    status = queryPort (sConnections s)

queryPort :: Connections -> Host -> Port -> PortStatus
queryPort connections host port = case Map.lookup port connections of
  Nothing -> Available
  Just (usedBy, _) -> if usedBy == host then Connected else InUse

connectPort :: Port -> Host -> ConnectionState -> IO ConnectionState
connectPort port host s = Ex.handle onConnectError $ do
  let bind = show port ++ ":localhost:" ++ show port

  M.void $ Concurrent.forkIO $ do
    (_, Just hOut, Just hErr, hProc) <-
      Proc.createProcess
        (Proc.proc "ssh" ["-N", "-L", bind, host])
          { Proc.std_out = Proc.CreatePipe,
            Proc.std_err = Proc.CreatePipe
          }
    BChan.writeBChan (sChan s) (PortConnected (port, host, hProc))

    handleOutput
      SubProcess
        { pHProc = hProc,
          pHOut = hOut,
          pHErr = hErr,
          pOnOut = \msg -> BChan.writeBChan (sChan s) (PortMessage (port, host, msg)),
          pOnErr = \err -> BChan.writeBChan (sChan s) (PortError (port, host, err))
        }

    M.void $ Proc.waitForProcess hProc
    BChan.writeBChan (sChan s) (PortDisconnected port)
  pure s
  where
    onConnectError :: Ex.IOException -> IO ConnectionState
    onConnectError _ = pure s

disconnectPort :: Port -> ConnectionState -> IO ConnectionState
disconnectPort port s = do
  case Map.lookup port (sConnections s) of
    Just (_, handle) -> do
      Proc.terminateProcess handle
      pure s
    Nothing -> pure s

data SubProcess = SubProcess
  { pHProc :: Proc.ProcessHandle,
    pHOut :: IO.Handle,
    pHErr :: IO.Handle,
    pOnOut :: String -> IO (),
    pOnErr :: String -> IO ()
  }

data Buffer = Buffer {bOut :: String, bErr :: String}

handleOutput :: SubProcess -> IO ()
handleOutput sub = handleOutput' sub Buffer {bOut = "", bErr = ""}

handleOutput' :: SubProcess -> Buffer -> IO ()
handleOutput' sub buf = do
  (bOut', out) <- getOutput (bOut buf) (pHOut sub)
  mapM_ (pOnOut sub) out
  (bErr', err) <- getOutput (bErr buf) (pHErr sub)
  mapM_ (pOnErr sub) err
  status <- Proc.getProcessExitCode (pHProc sub)
  case status of
    Nothing -> handleOutput' sub Buffer {bOut = bOut', bErr = bErr'}
    Just _ -> getLastOutput sub

getLastOutput :: SubProcess -> IO ()
getLastOutput sub = do
  out <- contents (pHOut sub)
  mapM_ (pOnOut sub) out
  err <- contents (pHErr sub)
  mapM_ (pOnErr sub) err
  where
    contents h = do
      bs <- BS.hGetContents h
      pure . lines . filter (/= '\r') . UTF8.toString $ bs

getOutput :: String -> IO.Handle -> IO (String, [String])
getOutput buf hOut = do
  bs <- BS.hGetNonBlocking hOut 256
  pure $ getLines (buf ++ (filter (/= '\r') . UTF8.toString $ bs))

getLines :: String -> (String, [String])
getLines "" = ("", [])
getLines buf =
  if (== '\n') . last $ buf
    then ("", ls)
    else (last ls, init ls)
  where
    ls = lines buf
