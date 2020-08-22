{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( tui
    ) where

import Lens.Micro
import Lens.Micro.TH
import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Core
  ( (<+>)
  , (<=>)
  , hLimit
  , vLimit
  , str
  )

import qualified Brick.Focus                as F
import qualified Brick.Widgets.Edit         as E
import qualified Brick.Widgets.Center       as C
import qualified Brick.Types                as T
import qualified Brick.Main                 as M
import qualified Graphics.Vty               as V

import Game

-- | Named resoureced
data Name = QuoteBox | TypeBox 
            deriving (Ord, Show, Eq)

-- | The state of the game
-- | We'll make a lens out of it, so that we have getters and setter
-- | QUESTION: Do we really need a EDIT box? ?
data TuiState =
    TuiState {  _game       :: Game 
             ,  _quoteBox   :: String
             ,  _typeBox    :: E.Editor String Name
             } deriving (Show)

makeLenses ''TuiState

-- | An attribute for HIT, Use this attribute when you have HIT action
hitAttrName :: AttrName 
hitAttrName = attrName "Hit"

missAttrName :: AttrName
missAttrName = attrName "Miss"

-- | Draw the character, If Miss color the character with RED
-- | str :: String -> Widget n
drawCharacter :: Character -> Widget ()
drawCharacter (Hit c)  = withAttr hitAttrName $ str [c]
drawCharacter (Miss c) = withAttr missAttrName $ str [c]

drawLine :: Line -> Widget ()
drawLine [] = str " "
drawLine line = foldl1 (<+>) $ map drawCharacter line 

-- | Takes the state, Creates the characters and draws the line
drawText :: TuiState -> Widget ()
drawText st = undefined

-- | The main application to draw the UI
tui :: IO ()
tui = do
  initialState <- buildInitialState
  endState <- defaultMain tuiApp initialState
  putStrLn $ show endState 

-- | The initial config to out APP, specifying various functions
-- | appDraw: Turns the current app state into a list of layers of type Widget
tuiApp :: App TuiState e Name
tuiApp =
  App
    { appDraw = drawTui
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleTuiEvent
    , appStartEvent = pure
    , appAttrMap = const $ attrMap mempty []
    }

-- | Initial state of the APP
buildInitialState :: IO TuiState
buildInitialState = do
    pure TuiState { _game = initialState "Hello"
                  , _quoteBox = "Quote to type"
                  , _typeBox = E.editor TypeBox Nothing ""
                  }

-- | The cursor as I understand is used to send the information from widget back to program, think of it like dbms cursor
-- appCursor :: TuiState -> [CursorLocation Name] -> Maybe (CursorLocation Name)
-- appCursor = F.focusRingCursor (^.focusRing)

-- | Change TuiState to drawable Widgets. This does the actual drawing
-- | `<=>` is a sugar for Vertical box Layout. Put widgets one above another
-- | `ts^.typeBox` means get the `_typeBox` constructor from TuiState
-- | FocusRing helps to keep focus on the Named Resource. This also gives cursor to the edit box
drawTui :: TuiState -> [Widget Name]
drawTui ts = [ui]
    where
        typeBox' =E.renderEditor (str . unlines) False (ts^.typeBox)
        ui = withBorderStyle unicode $
                borderWithLabel (str "TypeRacer") $ 
                    vBox [str (ts^.quoteBox), fill ' ', hBorder] <=>
                     (hLimit 30 $ vLimit 5 typeBox')


handleChar :: Char -> TuiState -> EventM Name (Next TuiState)
handleChar char ts = M.continue $ ts & game %~ (applyChar char)

-- | Handle the events. This is where Keyboard events will be captured.
-- | Continue takes a updated state and applies it
-- | `%~` modifies the value of a field. Like a setter
-- | `&` is the reverse of `$` ie. You give the argument first and then the function. Eg: num & add == add $ num
handleTuiEvent :: TuiState -> BrickEvent Name e -> EventM Name (Next TuiState)
handleTuiEvent ts (VtyEvent ev) = 
    case ev of
        V.EvKey V.KEsc [] -> M.halt ts
        -- V.EvKey (V.KChar '\t') [] -> M.continue $ ts & focusRing %~ F.focusNext
        -- V.EvKey V.KBackTab [] -> M.continue $ ts & focusRing %~ F.focusPrev
        V.EvKey (V.KChar c) [] -> handleChar c ts

handleTuiEvent ts _ = M.continue ts
