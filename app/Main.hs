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
    get,
    joinBorders,
    str,
    withBorderStyle,
    zoom,
    (<+>),
  )
import qualified Brick as Brick.Main
import Brick.Forms (Form (..), editTextField, handleFormEvent, newForm, renderForm, (@@=))
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Border.Style (unicode)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString.Lazy (ByteString, writeFile)
import qualified Data.Csv as CSV
import qualified Data.Text as T
import Graphics.Vty (Event (EvKey), Key (KEnter, KEsc), defAttr)
import Lens.Micro.Extras (view)
import Lens.Micro.TH (makeLenses)
import Text.Printf (printf)
import Types (Failure, Postcode (..), ReportData)
import Web.MembersApi (getReportData)

data FormState = FormState
  { _fileName :: T.Text
  }
  deriving (Show)

makeLenses ''FormState

data ResourceName = FormFileName
  deriving (Eq, Ord, Show)

makeForm :: FormState -> Form FormState () ResourceName
makeForm =
  newForm
    [ (str "File name: " <+>) @@= editTextField fileName FormFileName (Just 1)
    ]

generateUi :: ApplicationState -> Widget ResourceName
generateUi (ApplicationState form) =
  joinBorders $
    withBorderStyle unicode $
      borderWithLabel
        (str "MP Lookup v0.1")
        (renderForm form)

data ApplicationState = ApplicationState {_form :: Form FormState () ResourceName}

makeLenses ''ApplicationState

main :: IO ()
main = do
  let app =
        App
          { appDraw = \s -> [generateUi s],
            appChooseCursor = Brick.Main.showFirstCursor,
            appHandleEvent = eventHandler,
            appStartEvent = return (),
            appAttrMap = const $ attrMap defAttr []
          }
      initialFormState = FormState T.empty
      initialApplicationState = ApplicationState $ makeForm initialFormState
  ApplicationState frm <- defaultMain app initialApplicationState
  putStrLn $ printf "Final state: %s" $ show $ formState frm

eventHandler :: BrickEvent ResourceName () -> EventM ResourceName ApplicationState ()
eventHandler event = do
  currentApplicationState <- get
  let currentFormState = formState $ view form currentApplicationState
  case event of
    VtyEvent (EvKey KEsc []) -> Brick.Main.halt
    VtyEvent (EvKey KEnter []) -> liftIO $ doWork (T.unpack $ view fileName currentFormState)
    _ -> zoom form (handleFormEvent event)

doWork :: FilePath -> IO ()
doWork filePath = do
  postcodes <- readPostcodes filePath
  failuresOrReportData <- mapM getReportData postcodes
  let csvContents = foldMap createCsvRow failuresOrReportData
  Data.ByteString.Lazy.writeFile "resources/members.csv" csvContents
  putStrLn "Done!"

readPostcodes :: FilePath -> IO [Postcode]
readPostcodes filepath =
  map Postcode . lines <$> readFile filepath

createCsvRow :: Either Failure ReportData -> ByteString
createCsvRow eitherFailureOrReportData =
  case eitherFailureOrReportData of
    (Left failure) -> CSV.encode [failure]
    (Right reportData) -> CSV.encode [reportData]
