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
import Brick.Widgets.List (GenericList, list, listClear, listReplace, renderList)
import Brick.Widgets.Table (Table, renderTable, rowBorders, surroundingBorder, table)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Text as T
import Data.Vector (Vector, empty, fromList)
import Graphics.Vty (Event (EvKey), Key (KEnter, KEsc, KFun), defAttr)
import Lens.Micro.Platform (makeLenses, modifying, view)
import Text.Printf (printf)
import Types (AppFunctions (..), Constituency (..), ErrorMessage (..), Member (..), MpData (..), Postcode (..), SuccessMessage (MkSuccessMessage))

data FormState = FormState
  { _fileName :: T.Text
  }
  deriving (Show)

makeLenses ''FormState

data ResourceName = FormFileName | FormNewField | ResultList
  deriving (Eq, Ord, Show)

data ApplicationState = ApplicationState
  { _form :: Form FormState () ResourceName,
    _userMessage :: Maybe String,
    _list :: GenericList ResourceName Vector String,
    _mpLookupResults :: [Either ErrorMessage MpData]
  }

instance Show ApplicationState where
  show (ApplicationState form' maybeMessage _ _) = "ApplicationState {form = " ++ show (formState form') ++ ", userMessage = " ++ show maybeMessage ++ "}"

makeLenses ''ApplicationState

inputForm :: FormState -> Form FormState () ResourceName
inputForm =
  newForm
    [ (str "File name: " <+>) @@= editTextField fileName FormFileName (Just 1)
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

ui :: ApplicationState -> Widget ResourceName
ui applicationState =
  joinBorders $
    withBorderStyle unicode $
      renderTable $
        parentTable applicationState

eventHandler :: AppFunctions -> BrickEvent ResourceName () -> EventM ResourceName ApplicationState ()
eventHandler appFunctions brickEvent = do
  currentApplicationState <- get
  let currentFormState = formState $ view form currentApplicationState
  case brickEvent of
    VtyEvent (EvKey KEsc []) -> halt
    VtyEvent (EvKey KEnter []) -> do
      readResults <- liftIO $ readPostcodes appFunctions (T.unpack $ view fileName currentFormState)
      case readResults of
        (Right postCodes) -> do
          mpLookupResults' <- liftIO $ mapM (lookupMp appFunctions) postCodes
          modifying UserInterface.userMessage (const Nothing)
          modifying UserInterface.list $ listReplace (fromList $ map toListItem mpLookupResults') Nothing
          modifying UserInterface.mpLookupResults (const mpLookupResults')
        (Left (MkErrorMessage errorMessage)) -> do
          modifying UserInterface.userMessage (const $ Just errorMessage)
          modifying UserInterface.list listClear
      return ()
    VtyEvent (EvKey (KFun 1) []) -> do
      let mpLookupResults' = view UserInterface.mpLookupResults currentApplicationState
      writeResults <- liftIO $ writeCsv appFunctions "resources/members.csv" mpLookupResults'
      case writeResults of
        (Right (MkSuccessMessage successMessage)) -> modifying UserInterface.userMessage (const $ Just successMessage)
        (Left (MkErrorMessage errorMessage)) -> modifying UserInterface.userMessage (const $ Just errorMessage)
      return ()
    _ -> zoom form (handleFormEvent brickEvent)

app :: AppFunctions -> App ApplicationState () ResourceName
app appFunctions =
  App
    { appDraw = \s -> [ui s],
      appChooseCursor = showFirstCursor,
      appHandleEvent = eventHandler appFunctions,
      appStartEvent = return (),
      appAttrMap = const $ attrMap defAttr []
    }

initialApplicationState :: ApplicationState
initialApplicationState = ApplicationState (inputForm initialFormState) Nothing initialList []
  where
    initialFormState :: FormState
    initialFormState = FormState T.empty
    initialList :: GenericList ResourceName Vector String
    initialList = Brick.Widgets.List.list ResultList Data.Vector.empty 1

toListItem :: Either ErrorMessage MpData -> String
toListItem (Left errorMessage) = unErrorMessage errorMessage
toListItem (Right (MpData postcode constituency member)) =
  printf "%s (%s): %s" (getPostcode postcode) (getConstituencyName constituency) (getMemberName member)
