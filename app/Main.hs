{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Brick
  ( App
      ( App,
        appAttrMap,
        appChooseCursor,
        appDraw,
        appHandleEvent,
        appStartEvent
      ),
    BrickEvent (VtyEvent),
    EventM,
    Widget,
    attrMap,
    defaultMain,
    joinBorders,
    str,
    withBorderStyle,
    zoom,
    (<+>),
  )
import qualified Brick as Brick.Main
import Brick.Forms (Form (..), editTextField, handleFormEvent, newForm, renderForm)
import Brick.Widgets.Border (borderWithLabel, vBorder)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center (center)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Csv as CSV
import qualified Data.Text as T
import Graphics.Vty ( defAttr, Event(EvKey), Key(KEsc) )
import Lens.Micro.TH (makeLenses)
import Text.Printf (printf)
import Types (Failure, Postcode (..), ReportData)

data FormState = FormState
  { _fileName :: T.Text,
    _userName :: T.Text
  }
  deriving (Show)

makeLenses ''FormState

data ResourceName = FormFileName | FormUserName
  deriving (Eq, Ord, Show)

makeForm :: FormState -> Form FormState () ResourceName
makeForm =
  newForm
    [ editTextField fileName FormFileName Nothing,
      editTextField userName FormUserName Nothing
    ]

generateUi :: ApplicationState -> Widget ResourceName
generateUi (ApplicationState form) =
  joinBorders $
    withBorderStyle unicode $
      borderWithLabel
        (str "Hello world!")
        (center (str "Left") <+> vBorder <+> center (renderForm form))

data ApplicationState = ApplicationState {_form :: Form FormState () ResourceName}

makeLenses ''ApplicationState

main :: IO ()
main = do
  let app =
        App
          { appDraw = \s -> [generateUi s],
            appChooseCursor = Brick.Main.showFirstCursor,
            appHandleEvent = handleEvent,
            appStartEvent = return (),
            appAttrMap = const $ attrMap defAttr []
          }
      initialFormState = FormState (T.pack "Enter file name") (T.pack "Enter user name")
      initialApplicationState = ApplicationState $ makeForm initialFormState
  ApplicationState frm <- defaultMain app initialApplicationState
  putStrLn $ printf "Final state: %s" $ show $ formState frm

handleEvent :: BrickEvent ResourceName () -> EventM ResourceName ApplicationState ()
handleEvent event = do
  case event of
    VtyEvent (EvKey KEsc []) -> Brick.Main.halt
    _ -> zoom form $ handleFormEvent event

readPostcodes :: IO [Postcode]
readPostcodes =
  map Postcode . lines <$> readFile "resources/postcodes.txt"

createCsvRow :: Either Failure ReportData -> ByteString
createCsvRow eitherFailureOrReportData =
  case eitherFailureOrReportData of
    (Left failure) -> CSV.encode [failure]
    (Right reportData) -> CSV.encode [reportData]
