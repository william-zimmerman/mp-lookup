{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module UserInterface (app, initialApplicationState) where

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
    get,
    halt,
    joinBorders,
    str,
    withBorderStyle,
    zoom,
    (<+>), showCursorNamed,
  )
import Brick.Forms (Form (..), editTextField, handleFormEvent, newForm, renderForm, (@@=))
import Brick.Widgets.Border (borderWithLabel, hBorder)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Core (emptyWidget, hLimitPercent, vLimitPercent)
import Brick.Widgets.List (GenericList, list, listReplace, renderList)
import Brick.Widgets.Table (Table, renderTable, rowBorders, surroundingBorder, table)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Text as T
import Data.Vector (Vector, empty, fromList)
import Graphics.Vty (Event (EvKey), Key (KEnter, KEsc, KFun), defAttr)
import Lens.Micro.Platform (Lens', lens, makeLenses, modifying, view, over)
import Text.Printf (printf)
import Types (AppFunctions (..), Constituency (..), ErrorMessage (..), Member (..), MpData (..), Postcode (..), SuccessMessage (MkSuccessMessage))
import Brick.Widgets.Dialog (dialog, Dialog, renderDialog)

data ResourceName = InputFileForm | FormNewField | ResultList | OutputFileForm
  deriving (Eq, Ord, Show)

stringTextLens :: Lens' String T.Text
stringTextLens = lens T.pack (\_ b -> T.unpack b)

data ApplicationState = ApplicationState
  { _inputFileForm :: Form String () ResourceName,
    _outputFileForm :: Form String () ResourceName,
    _userMessage :: Maybe String,
    _list :: GenericList ResourceName Vector String,
    _mpLookupResults :: [Either ErrorMessage MpData],
    _focusTarget :: ResourceName
  }

instance Show ApplicationState where
  show (ApplicationState inputFileForm' _ maybeMessage _ _ _) = "ApplicationState {inputFileForm = " ++ show (formState inputFileForm') ++ ", userMessage = " ++ show maybeMessage ++ "}"

makeLenses ''ApplicationState

createInputFileForm :: String -> Form String () ResourceName
createInputFileForm =
  newForm
    [ (str "Input file name: " <+>) @@= editTextField stringTextLens InputFileForm (Just 1)
    ]

createOutputFileForm :: String -> Form String () ResourceName
createOutputFileForm =
  newForm
    [ (str "Output file name: " <+>) @@= editTextField stringTextLens OutputFileForm (Just 1)
    ]

parentTable :: ApplicationState -> Table ResourceName
parentTable applicationState =
  surroundingBorder False $
    rowBorders False $
      table
        [ [hLimitPercent 100 $ vLimitPercent 98 $ borderWithLabel (str "MP Lookup v0.1") $ renderTable (childTable applicationState)],
          [hLimitPercent 100 $ str $ availableCommands applicationState]
        ]

availableCommands :: ApplicationState -> String
availableCommands _ = " esc: exit | f1: write to file"

childTable :: ApplicationState -> Table ResourceName
childTable (ApplicationState inputFileForm' _ maybeMessage list' _ _) =
  let listHasFocus = False
   in surroundingBorder False $
        rowBorders False $
          table
            [ [hLimitPercent 100 $ renderForm inputFileForm'],
              [hLimitPercent 100 $ hBorder],
              [hLimitPercent 100 $ maybe emptyWidget str maybeMessage],
              [hLimitPercent 100 $ vLimitPercent 100 $ renderList renderListFunc listHasFocus list']
            ]

renderListFunc :: Bool -> String -> Widget ResourceName
renderListFunc _ = str

baseUiLayer :: ApplicationState -> Widget ResourceName
baseUiLayer applicationState =
  joinBorders $
    withBorderStyle unicode $
      renderTable $
        parentTable applicationState

dialogUiLayer :: ApplicationState -> Widget ResourceName
dialogUiLayer applicationState = 
  let
    showDialog = (view focusTarget applicationState) == OutputFileForm
    outputFileDialog :: Dialog () ResourceName
    outputFileDialog = dialog (Just $ str "Input needed") Nothing 100
    bodyWidget = renderForm (view outputFileForm applicationState)
  in
    if showDialog
    then renderDialog outputFileDialog bodyWidget
    else emptyWidget

eventHandler :: AppFunctions -> BrickEvent ResourceName () -> EventM ResourceName ApplicationState ()
eventHandler functions brickEvent = do
  applicationState <- get
  case brickEvent of
    VtyEvent (EvKey KEsc []) -> halt
    VtyEvent (EvKey KEnter []) -> executeReadAndLookup functions applicationState
    VtyEvent (EvKey (KFun 1) []) -> showDialog applicationState
    _ ->
      if (view focusTarget applicationState == OutputFileForm)
      then zoom outputFileForm (handleFormEvent brickEvent)
      else zoom inputFileForm (handleFormEvent brickEvent)

executeReadAndLookup :: AppFunctions -> ApplicationState -> EventM ResourceName ApplicationState ()
executeReadAndLookup functions applicationState =
  let postcodeFilePath = formState $ view inputFileForm applicationState
      readPostcodesFunc = readPostcodes functions
      lookupMpFunc = lookupMp functions
   in do
        readResults <- liftIO $ readPostcodesFunc postcodeFilePath
        either
          updateUiWithErrorMessage
          ( \postcodes -> do
              lookupResults <- liftIO $ mapM lookupMpFunc postcodes
              updateUiWithLookupResults lookupResults
          )
          readResults

updateUiWithErrorMessage :: ErrorMessage -> EventM ResourceName ApplicationState ()
updateUiWithErrorMessage (MkErrorMessage errorMessage) =
  modifying UserInterface.userMessage (const $ Just errorMessage)

updateUiWithSuccessMessage :: SuccessMessage -> EventM ResourceName ApplicationState ()
updateUiWithSuccessMessage (MkSuccessMessage message) =
  modifying UserInterface.userMessage (const $ Just message)

updateUiWithLookupResults :: [Either ErrorMessage MpData] -> EventM ResourceName ApplicationState ()
updateUiWithLookupResults results = do
  modifying UserInterface.userMessage (const Nothing)
  modifying UserInterface.list $ listReplace (fromList $ map toListItem results) Nothing
  modifying UserInterface.mpLookupResults (const results)

showDialog :: ApplicationState -> EventM ResourceName ApplicationState ()
showDialog applicationState =
  modifying focusTarget (const OutputFileForm)

executeWriteToFile :: AppFunctions -> ApplicationState -> EventM ResourceName ApplicationState ()
executeWriteToFile functions applicationState =
  let lookupResults = view UserInterface.mpLookupResults applicationState
      writeFunc = writeCsv functions
   in do
        writeResults <- liftIO $ writeFunc "resources/members.csv" lookupResults
        either
          updateUiWithErrorMessage
          updateUiWithSuccessMessage
          writeResults

app :: AppFunctions -> App ApplicationState () ResourceName
app appFunctions =
  App
    { appDraw = \s -> [dialogUiLayer s, baseUiLayer s],
      appChooseCursor = showCursorNamed . view focusTarget,
      appHandleEvent = eventHandler appFunctions,
      appStartEvent = return (),
      appAttrMap = const $ attrMap defAttr []
    }

initialApplicationState :: ApplicationState
initialApplicationState = ApplicationState (createInputFileForm []) (createOutputFileForm []) Nothing initialList [] InputFileForm
  where
    initialList :: GenericList ResourceName Vector String
    initialList = Brick.Widgets.List.list ResultList Data.Vector.empty 1

toListItem :: Either ErrorMessage MpData -> String
toListItem (Left errorMessage) = unErrorMessage errorMessage
toListItem (Right (MpData postcode constituency member)) =
  printf "%s (%s): %s" (getPostcode postcode) (getConstituencyName constituency) (getMemberName member)
