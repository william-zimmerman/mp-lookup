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
    halt,
    joinBorders,
    showFirstCursor,
    str,
    withBorderStyle,
    zoom,
    (<+>),
  )
import Brick.Forms (Form (..), editTextField, handleFormEvent, newForm, renderForm, (@@=))
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Core (emptyWidget, hLimitPercent)
import Brick.Widgets.Table (Table, renderTable, rowBorders, surroundingBorder, table)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString.Lazy (ByteString, writeFile)
import qualified Data.Csv as CSV
import qualified Data.Text as T
import Graphics.Vty (Event (EvKey), Key (KEnter, KEsc), defAttr)
import Lens.Micro.Platform (makeLenses, modifying, over, view)
import System.Directory (doesFileExist)
import Text.Printf (printf)
import Types (Failure, Postcode (..), ReportData)
import Web.MembersApi (getReportData)

newtype FormState = FormState
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

mainTable :: ApplicationState -> Table ResourceName
mainTable (ApplicationState form maybeMessage) =
  surroundingBorder False $
    rowBorders False $
      table
        [ [hLimitPercent 100 $ borderWithLabel (str "MP Lookup v0.1") $ renderForm form],
          [hLimitPercent 100 messageWidget]
        ]
  where
    messageWidget :: Widget ResourceName
    messageWidget = maybe emptyWidget str maybeMessage

generateUi :: ApplicationState -> Widget ResourceName
generateUi applicationState =
  joinBorders $
    withBorderStyle unicode $
      renderTable $
        mainTable applicationState

data ApplicationState = ApplicationState
  { _form :: Form FormState () ResourceName,
    _userMessage :: Maybe String
  }

makeLenses ''ApplicationState

main :: IO ()
main = do
  let app =
        App
          { appDraw = \s -> [generateUi s],
            appChooseCursor = showFirstCursor,
            appHandleEvent = eventHandler,
            appStartEvent = return (),
            appAttrMap = const $ attrMap defAttr []
          }
      initialFormState = FormState T.empty
      initialApplicationState = ApplicationState (makeForm initialFormState) Nothing
  ApplicationState frm _ <- defaultMain app initialApplicationState
  putStrLn $ printf "Final state: %s" $ show $ formState frm

eventHandler :: BrickEvent ResourceName () -> EventM ResourceName ApplicationState ()
eventHandler event = do
  currentApplicationState <- get
  let currentFormState = formState $ view form currentApplicationState
  case event of
    VtyEvent (EvKey KEsc []) -> halt
    VtyEvent (EvKey KEnter []) -> do
      results <- liftIO $ doWork (T.unpack $ view fileName currentFormState)
      case results of
        Success -> modifying userMessage (const $ Just "Done!")
        Failure errorMessage -> modifying userMessage (const $ Just errorMessage)
      return ()
    _ -> zoom form (handleFormEvent event)

type ErrorMessage = String

data Outcome = Success | Failure ErrorMessage

doWork :: FilePath -> IO Outcome
doWork filePath = do
  fileExists <- doesFileExist filePath
  if not fileExists
    then return $ Failure (printf "File %s does not exist" $ show filePath)
    else do
      postcodes <- readPostcodes filePath
      failuresOrReportData <- mapM getReportData postcodes
      let csvContents = foldMap createCsvRow failuresOrReportData
      Data.ByteString.Lazy.writeFile "resources/members.csv" csvContents
      return Success

readPostcodes :: FilePath -> IO [Postcode]
readPostcodes filepath =
  map Postcode . lines <$> readFile filepath

createCsvRow :: Either Failure ReportData -> ByteString
createCsvRow eitherFailureOrReportData =
  case eitherFailureOrReportData of
    (Left failure) -> CSV.encode [failure]
    (Right reportData) -> CSV.encode [reportData]
