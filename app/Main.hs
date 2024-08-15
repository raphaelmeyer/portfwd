module Main where

import qualified Brick.AttrMap as Attr
import qualified Brick.Main as Brick
import qualified Brick.Types as Types
import qualified Brick.Util as Util
import qualified Brick.Widgets.Border as Border
import qualified Brick.Widgets.Core as Core
import qualified Control.Monad as M (void)
import qualified Data.Map.Strict as Map
import qualified Graphics.Vty as Vty
import qualified Settings
import qualified Text.JSON

type Host = String

type Port = Int

data SelectHost = SelectHost
  { selectLeft :: [Host],
    selectRight :: [Host]
  }
  deriving (Eq, Show)

data ConnectionState = ConnectionState
  { sHosts :: SelectHost,
    sPorts :: [Port],
    sConnections :: Map.Map Port Host
  }
  deriving (Eq, Show)

type Name = ()

main :: IO ()
main = do
  M.void $ Brick.defaultMain app initialState

initialState :: ConnectionState
initialState =
  case result of
    Text.JSON.Ok settings ->
      ConnectionState
        { sHosts = SelectHost {selectLeft = [], selectRight = Settings.hosts settings},
          sPorts = Settings.ports settings,
          sConnections = Map.empty
        }
    Text.JSON.Error _ ->
      ConnectionState
        { sHosts = SelectHost {selectLeft = [], selectRight = []},
          sPorts = [],
          sConnections = Map.empty
        }
  where
    result = Text.JSON.decode "{\"hosts\": [\"foo\", \"pan\"], \"ports\": [1234,5555,7890]}" :: Text.JSON.Result Settings.Settings

app :: Brick.App ConnectionState e Name
app =
  Brick.App
    { Brick.appDraw = drawUI,
      Brick.appChooseCursor = Brick.neverShowCursor,
      Brick.appHandleEvent = handleEvent,
      Brick.appStartEvent = pure (),
      Brick.appAttrMap = const attributes
    }

mapHosts :: (Bool -> Host -> a) -> SelectHost -> [a]
mapHosts f SelectHost {selectLeft = left, selectRight = []} =
  map (f False) . reverse $ left
mapHosts f SelectHost {selectLeft = left, selectRight = (current : right)} =
  (map (f False) . reverse $ left) ++ [f True current] ++ map (f False) right

nextHost :: SelectHost -> SelectHost
nextHost s@SelectHost {selectLeft = left, selectRight = right} =
  case (left, right) of
    (_, []) -> s
    (_, [_]) -> s
    (l, current : r) -> SelectHost {selectLeft = current : l, selectRight = r}

previousHost :: SelectHost -> SelectHost
previousHost s@SelectHost {selectLeft = left, selectRight = right} =
  case (left, right) of
    ([], _) -> s
    (current : l, r) -> SelectHost {selectLeft = l, selectRight = current : r}

drawUI :: ConnectionState -> [Types.Widget Name]
drawUI s = [Core.hBox . mapHosts (drawHost (sPorts s)) . sHosts $ s]

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
    let s' = s {sHosts = nextHost (sHosts s)}
    Types.put s'
  Vty.EvKey Vty.KLeft [] -> do
    s <- Types.get
    let s' = s {sHosts = previousHost (sHosts s)}
    Types.put s'
  _ -> pure ()
handleEvent _ = pure ()
