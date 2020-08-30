{-
The code is highly motivated from 
https://github.com/callum-oakley/gotta-go-fast/blob/master/src/UI.hs
-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module UI
    ( run
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
import qualified Data.Text                  as T

import           Control.Monad.IO.Class (liftIO)
import           Data.Char              (isSpace)
import           Data.Maybe             (fromMaybe)
import           Data.Time              (getCurrentTime)
import           Data.Word              (Word8)
import           Text.Wrap              (WrapSettings (..), wrapText, wrapTextToLines)
import           Data.List.Split        (chunksOf)
import           Game

-- | Named resoureced
type Name = () 

-- | The state of the game
-- | We'll make a lens out of it, so that we have getters and setter
data TuiState =
    TuiState {  _game       :: Game 
             } deriving (Show)

makeLenses ''TuiState

-- | An attribute for HIT, Use this attribute when you have HIT action
hitAttrName :: AttrName 
hitAttrName = attrName "Hit"

missAttrName :: AttrName
missAttrName = attrName "Miss"

emptyAttrName :: AttrName
emptyAttrName = attrName "Empty"

-- | Draw the character, If Miss color the character with RED
-- | str :: String -> Widget n
drawCharacter :: Character -> Widget Name
drawCharacter (Hit c)  = withAttr hitAttrName $ str [c]
drawCharacter (Miss ' ') = withAttr missAttrName $ str ['_']
drawCharacter (Miss c) = withAttr missAttrName $ str [c]
drawCharacter (Empty c) = withAttr emptyAttrName $ str [c]

drawLine :: Line -> Widget Name
drawLine [] = emptyWidget
drawLine line = foldl1 (<+>) $ map drawCharacter line 

-- | Wrap Quote at 80 characters.
-- | As soon as you reach 80 char add a vertical box below it and let the text be written there.
-- TODO: Any better version of wrapping? Cases are present where a long word is divided into two parts. How to fix it
drawQuote :: TuiState -> Widget Name
drawQuote ts =  C.center. padAll 1 $ foldl (<=>) emptyWidget $  map drawLine lines
  where lines = chunksOf wrapWidth' characters
        characters = character quote' input'
        quote' = (ts ^. game) ^. quote
        input' = (ts ^. game) ^. input
        wrapWidth' =  (ts ^. game) ^. wrapWidth

-- | Takes the state, Creates the characters and draws the line
-- Is there a better way to write the lenses.
drawInputText :: TuiState -> Widget Name
drawInputText ts = foldl (<=>) emptyWidget $ map txt wrappedInput
  where wrappedInput = T.chunksOf wrapWidth' input'
        input'       = unInput $ (ts ^. game) ^. input
        wrapWidth' =  (ts ^. game) ^. wrapWidth
        
        
drawResult :: TuiState -> Widget Name
drawResult ts = str $ 
  (show . round $ wpm $ ts^.game) ++ " words per minute is " ++ 
  (show . round $ accuracy (ts^.game) * 100) ++ "% accuracy"

-- | Change TuiState to drawable Widgets. This does the actual drawing
-- | `<=>` is a sugar for Vertical box Layout. Put widgets one above another
drawTui :: TuiState -> [Widget Name]
drawTui ts 
  | gameEnded = pure. C.center . padAll 1 $ drawResult ts
  | otherwise = [ui]
    where
        gameEnded = hasEnded (ts^.game)
        ui = withBorderStyle unicode $
                borderWithLabel (str "TypeRacer") $ 
                    vBox [(drawQuote ts), fill ' ', hBorder] 
                    <=> showCursor () (Location $ cursor (ts^.game)) (vBox [(drawInputText ts), fill ' '])

-- | Handle the characters from Keyboard
-- | After applying each char check if the game has ended, If yes then stop clock else continue
handleChar :: Char -> TuiState -> EventM Name (Next TuiState)
handleChar char ts = 
  if isComplete game' 
    then do
      now <- liftIO getCurrentTime
      M.continue $ ts & game %~ (stopClock now)
    else M.continue $ ts & game %~ (applyChar char)
  where game' = applyChar char (ts^.game)
   

-- | Handle the events. This is where Keyboard events will be captured.
-- | Continue takes a updated state and applies it
-- | `%~` modifies the value of a field. Like a setter
-- | `&` is the reverse of `$` ie. You give the argument first and then the function. Eg: num & add == add $ num
handleTuiEvent :: TuiState -> BrickEvent Name e -> EventM Name (Next TuiState)
handleTuiEvent ts (VtyEvent (V.EvKey key []))
  | hasEnded (ts^.game) = 
      case key of
        V.KEnter -> M.halt ts
        _        -> M.continue ts
  | otherwise = 
      case key of
        V.KChar c -> handleChar c ts
        V.KEsc    -> M.halt ts
        V.KBS     -> M.continue $ ts & game %~ applyBackSpace
        _         -> M.continue ts
handleTuiEvent ts _ = M.continue ts

-- | Initial state of the APP
-- | Start the clock when the game starts
buildInitialState :: String -> IO TuiState
buildInitialState quote' = do
    let gameInitialState = initialState quote'
    now <- liftIO getCurrentTime
    let game' =  startClock now gameInitialState
    pure TuiState { _game = game'}


-- | The initial config to out APP, specifying various functions
-- | appDraw: Turns the current app state into a list of layers of type Widget
tuiApp :: V.Attr -> V.Attr -> V.Attr -> App TuiState e Name
tuiApp hitAttr missAttr emptyAttr=
  App
    { appDraw = drawTui
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleTuiEvent
    , appStartEvent = pure
    , appAttrMap = const $ 
                   attrMap 
                      V.defAttr
                      [ (hitAttrName, hitAttr)
                      , (missAttrName, missAttr)
                      , (emptyAttrName, emptyAttr)
                      ]
    }

-- | The main application to draw the UI
run :: Maybe Word8 -> Maybe Word8 -> Maybe Word8 -> String ->  IO ()
run fgEmptyCode fgErrorCode fgCorrectCode quote'= do
  initialState <- buildInitialState quote'
  endState <- defaultMain (tuiApp hitAttr missAttr emptyAttr) initialState
  putStrLn $ show endState 
    where 
      hitAttr     = fg . V.ISOColor $ fromMaybe 2 fgCorrectCode
      emptyAttr   = fg . V.ISOColor  $ fromMaybe 8 fgEmptyCode
      missAttr    = on missAttrFG missAttrBG
      missAttrFG  = V.ISOColor $ fromMaybe 1 fgErrorCode
      missAttrBG  = V.ISOColor $ 7
