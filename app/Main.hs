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

data ConnectionState = ConnectionState
  { sHosts :: [String],
    sPorts :: [Int],
    sConnections :: Map.Map String Int
  }
  deriving (Eq, Show)

type Name = ()

main :: IO ()
main = do
  M.void $ Brick.defaultMain app initialState

initialState :: ConnectionState
initialState =
  ConnectionState
    { sHosts = ["foo", "bar", "abcdefghijklmnopqrstu"],
      sPorts = [1234, 4456, 4000, 4001, 4002],
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
drawUI s = [Core.hBox . map hosts . sHosts $ s]
  where
    hosts h = drawHost h (sPorts s)

drawHost :: String -> [Int] -> Types.Widget Name
drawHost host ports = if host == "bar" then Core.withAttr (Attr.attrName "selectedHost") box else box
  where
    box = Core.padLeftRight 1 . Border.border . Core.hLimit 16 . Core.vBox $ Core.str host : Border.hBorder : map drawPort ports

drawPort :: Int -> Types.Widget Name
drawPort = Core.padLeft (Core.Pad 2) . Core.str . show

attributes :: Attr.AttrMap
attributes =
  Attr.attrMap
    Vty.defAttr
    [ (Attr.attrName "selectedHost", Util.fg Vty.yellow)
    ]

handleEvent :: Types.BrickEvent Name e -> Types.EventM Name ConnectionState ()
handleEvent (Types.VtyEvent ev) = case ev of
  Vty.EvKey Vty.KEsc [] -> Brick.halt
  Vty.EvKey (Vty.KChar 'q') [] -> Brick.halt
  Vty.EvKey Vty.KLeft [] -> do
    s <- Types.get
    let s' = s {sPorts = 1111 : sPorts s}
    Types.put s'
  _ -> pure ()
handleEvent _ = pure ()
