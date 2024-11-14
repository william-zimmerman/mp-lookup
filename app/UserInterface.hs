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
    showFirstCursor,
    str,
    withBorderStyle,
    zoom,
    (<+>),
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
import Lens.Micro.Platform (Lens', lens, makeLenses, modifying, view)
import Text.Printf (printf)
import Types (AppFunctions (..), Constituency (..), ErrorMessage (..), Member (..), MpData (..), Postcode (..), SuccessMessage (MkSuccessMessage))

data ResourceName = FormFileName | FormNewField | ResultList
  deriving (Eq, Ord, Show)

stringTextLens :: Lens' String T.Text
stringTextLens = lens T.pack (\_ b -> T.unpack b)

data ApplicationState = ApplicationState
  { _form :: Form String () ResourceName,
    _userMessage :: Maybe String,
    _list :: GenericList ResourceName Vector String,
    _mpLookupResults :: [Either ErrorMessage MpData]
  }

instance Show ApplicationState where
  show (ApplicationState form' maybeMessage _ _) = "ApplicationState {form = " ++ show (formState form') ++ ", userMessage = " ++ show maybeMessage ++ "}"

makeLenses ''ApplicationState

inputForm :: String -> Form String () ResourceName
inputForm =
  newForm
    [ (str "File name: " <+>) @@= editTextField stringTextLens FormFileName (Just 1)
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
childTable (ApplicationState form' maybeMessage list' _) =
  let listHasFocus = False
   in surroundingBorder False $
        rowBorders False $
          table
            [ [hLimitPercent 100 $ renderForm form'],
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

eventHandler :: AppFunctions -> BrickEvent ResourceName () -> EventM ResourceName ApplicationState ()
eventHandler functions brickEvent = do
  applicationState <- get
  case brickEvent of
    VtyEvent (EvKey KEsc []) -> halt
    VtyEvent (EvKey KEnter []) -> executeReadAndLookup functions applicationState
    VtyEvent (EvKey (KFun 1) []) -> executeWriteToFile functions applicationState
    _ -> zoom form (handleFormEvent brickEvent)

executeReadAndLookup :: AppFunctions -> ApplicationState -> EventM ResourceName ApplicationState ()
executeReadAndLookup functions applicationState =
  let postcodeFilePath = formState $ view form applicationState
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
    { appDraw = \s -> [baseUiLayer s],
      appChooseCursor = showFirstCursor,
      appHandleEvent = eventHandler appFunctions,
      appStartEvent = return (),
      appAttrMap = const $ attrMap defAttr []
    }

initialApplicationState :: ApplicationState
initialApplicationState = ApplicationState (inputForm initialFormState) Nothing initialList []
  where
    initialFormState :: String
    initialFormState = []
    initialList :: GenericList ResourceName Vector String
    initialList = Brick.Widgets.List.list ResultList Data.Vector.empty 1

toListItem :: Either ErrorMessage MpData -> String
toListItem (Left errorMessage) = unErrorMessage errorMessage
toListItem (Right (MpData postcode constituency member)) =
  printf "%s (%s): %s" (getPostcode postcode) (getConstituencyName constituency) (getMemberName member)
