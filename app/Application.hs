module Application where

import qualified Brick.BChan as BChan
import qualified Brick.Main as Brick
import qualified Brick.Types as Types
import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Ex
import qualified Control.Monad as M (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import qualified Graphics.Vty as Vty
import qualified Lens.Micro as Lens
import qualified Settings
import qualified SubProcess as Sub
import qualified System.Process as Proc
import Types (Host, Port)
import qualified UI

type Connections = Map.Map Port (Host, Proc.ProcessHandle)

data ApplicationEvent
  = PortConnected (Port, Host, Proc.ProcessHandle)
  | PortDisconnected Port
  | MessageReceived (Port, Host, String)
  | ErrorReceived (Port, Host, String)

data ApplicationState = ApplicationState
  { appStateUI :: UI.UIState,
    sConnections :: Connections,
    sChan :: BChan.BChan ApplicationEvent
  }

runApplication :: Settings.Settings -> IO ()
runApplication settings = do
  chan <- BChan.newBChan 8
  (_, vty) <- Brick.customMainWithDefaultVty (Just chan) app (initialState settings chan)
  Vty.shutdown vty

onError :: Ex.IOException -> IO ()
onError = showError

showError :: (Show s) => s -> IO ()
showError err = putStrLn $ "❗ " ++ show err

initialState :: Settings.Settings -> BChan.BChan ApplicationEvent -> ApplicationState
initialState settings chan =
  ApplicationState
    { appStateUI = UI.mkUIState settings,
      sConnections = Map.empty,
      sChan = chan
    }

app :: Brick.App ApplicationState ApplicationEvent UI.Name
app =
  Brick.App
    { Brick.appDraw = UI.draw . appStateUI,
      Brick.appChooseCursor = Brick.neverShowCursor,
      Brick.appHandleEvent = handleEvent,
      Brick.appStartEvent = pure (),
      Brick.appAttrMap = const UI.attributes
    }

lensUIState :: Lens.Lens' ApplicationState UI.UIState
lensUIState = Lens.lens appStateUI (\appState ui -> appState {appStateUI = ui})

handleEvent :: Types.BrickEvent UI.Name ApplicationEvent -> Types.EventM UI.Name ApplicationState ()
handleEvent (Types.VtyEvent ev) = case ev of
  Vty.EvKey Vty.KEsc [] -> shutdownApp
  Vty.EvKey (Vty.KChar 'q') [] -> shutdownApp
  Vty.EvKey Vty.KLeft [] -> Types.zoom lensUIState . Types.modify $ UI.onLeft
  Vty.EvKey Vty.KRight [] -> do
    Types.zoom lensUIState . Types.modify $ UI.onRight
  Vty.EvKey Vty.KUp [] -> do
    Types.zoom lensUIState . Types.modify $ UI.onUp
  Vty.EvKey Vty.KDown [] -> do
    Types.zoom lensUIState . Types.modify $ UI.onDown
  Vty.EvKey Vty.KEnter [] -> do
    s <- Types.get
    liftIO . togglePort $ s
  _ -> pure ()
handleEvent (Types.AppEvent ev) = case ev of
  (PortConnected (port, host, handle)) -> do
    Types.zoom lensUIState . Types.modify $ UI.onPortConnected port host
    Types.modify (\s -> s {sConnections = Map.insert port (host, handle) (sConnections s)})
  (PortDisconnected port) -> do
    Types.zoom lensUIState . Types.modify $ UI.onPortDisconnected port
    Types.modify (\s -> s {sConnections = Map.delete port (sConnections s)})
  (MessageReceived (port, host, message)) ->
    Types.zoom lensUIState . Types.modify $ UI.onMessageReceived port host message
  (ErrorReceived (port, host, message)) ->
    Types.zoom lensUIState . Types.modify $ UI.onErrorReceived port host message
handleEvent _ = pure ()

shutdownApp :: Types.EventM UI.Name ApplicationState ()
shutdownApp = do
  s <- Types.get
  mapM_ (liftIO . (`disconnectPort` s)) (Map.keys . sConnections $ s)
  Brick.halt

togglePort :: ApplicationState -> IO ()
togglePort s = do
  case UI.getSelected . appStateUI $ s of
    Just (host, port, status) -> action s
      where
        action = case status of
          UI.Available -> connectPort port host
          UI.Connected -> disconnectPort port
          _ -> pure . const ()
    Nothing -> pure ()

connectPort :: Port -> Host -> ApplicationState -> IO ()
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

    Sub.handleOutput
      Sub.SubProcess
        { Sub.pHProc = hProc,
          Sub.pHOut = hOut,
          Sub.pHErr = hErr,
          Sub.pOnOut = \msg -> BChan.writeBChan (sChan s) . MessageReceived $ (port, host, msg),
          Sub.pOnErr = \err -> BChan.writeBChan (sChan s) . ErrorReceived $ (port, host, err)
        }

    M.void $ Proc.waitForProcess hProc
    BChan.writeBChan (sChan s) (PortDisconnected port)
  where
    onConnectError :: Ex.IOException -> IO ()
    onConnectError _ = pure ()

disconnectPort :: Port -> ApplicationState -> IO ()
disconnectPort port s = do
  case Map.lookup port (sConnections s) of
    Just (_, handle) -> do
      Proc.terminateProcess handle
      pure ()
    Nothing -> pure ()
