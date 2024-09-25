module Main (main) where

import Brick
    ( App(App, appAttrMap, appDraw, appChooseCursor, appHandleEvent,
          appStartEvent),
      Widget,
      attrMap,
      joinBorders,
      withBorderStyle,
      str,
      (<+>),
      defaultMain )
import qualified Brick as Brick.Main
import Brick.Widgets.Border (borderWithLabel, vBorder)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center (center)
import Data.ByteString.Lazy (ByteString, writeFile)
import qualified Data.Csv as CSV
import Graphics.Vty.Attributes (defAttr)
import Types (Failure, Postcode (..), ReportData)
import Web.MembersApi (getReportData)

main :: IO ()
main = do
  let app =
        App
          { appDraw = const [simpleUi],
            appChooseCursor = Brick.Main.neverShowCursor,
            appHandleEvent = const Brick.Main.halt,
            appStartEvent = return (),
            appAttrMap = const $ attrMap defAttr []
          }
      initialState = ()
  defaultMain app initialState

simpleUi :: Widget ()
simpleUi =
  joinBorders $
    withBorderStyle unicode $
      borderWithLabel (str "Hello world!") $
        (center (str "Left") <+> vBorder <+> center (str "Right"))

readPostcodes :: IO [Postcode]
readPostcodes =
  map Postcode . lines <$> readFile "resources/postcodes.txt"

createCsvRow :: Either Failure ReportData -> ByteString
createCsvRow eitherFailureOrReportData =
  case eitherFailureOrReportData of
    (Left failure) -> CSV.encode [failure]
    (Right reportData) -> CSV.encode [reportData]
