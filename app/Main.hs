module Main where

import qualified Brick
import qualified Brick.Widgets.Border as WB
import qualified Brick.Widgets.Core as WC
import qualified Control.Monad as M (void)
import qualified Graphics.Vty

data ConnectionState = ConnectionState {sHosts :: [String], sPorts :: [Int]} deriving (Eq, Show)

type Name = ()

main :: IO ()
main = do
  M.void $ Brick.defaultMain app initialState

initialState :: ConnectionState
initialState = ConnectionState {sHosts = ["foo", "bar"], sPorts = [1234, 4456]}

app :: Brick.App ConnectionState e Name
app =
  Brick.App
    { Brick.appDraw = drawUI,
      Brick.appChooseCursor = Brick.neverShowCursor,
      Brick.appHandleEvent = undefined,
      Brick.appStartEvent = pure (),
      Brick.appAttrMap = const attributes
    }

drawUI :: ConnectionState -> [Brick.Widget Name]
drawUI s =
  [ WC.vBox
      [ WC.hBox . map Brick.str . sHosts $ s,
        WB.hBorder,
        WC.hBox . map (Brick.str . show) . sPorts $ s
      ]
  ]

attributes :: Brick.AttrMap
attributes = Brick.attrMap Graphics.Vty.defAttr []
