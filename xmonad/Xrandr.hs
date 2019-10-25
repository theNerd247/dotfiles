{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Main where

import Control.Applicative
import Control.Arrow
import Control.Monad ((>=>))
import Data.Attoparsec.Text
import Data.Bifunctor (bimap)
import Data.Foldable (foldMap)
import Data.Functor.Foldable
import Data.List (partition, find, sortOn)
import Data.Maybe (listToMaybe)
import Data.Monoid
import Data.String
import Numeric.Natural
import Prelude hiding (takeWhile, take)
import Shelly (shelly, run, run_)
import qualified Data.Text as T

-- | Requirements
--
--  * Screens can be either: Connected or Disconnected
--  * Connected Screens can be Enabled or Disabled
--  * Enabled screens must have a Mode and Rotation defined
--  * Default rotation is Normal which means the screen is upright).
--  * Only 1 screen can be the Primary, and all the others must be Secondary
--  * Disabled / Disconnected screens must not have a position
--  * Connected Screens must have a position relative to another screen
--      Secondary -> Primary | Secondary; Primary doesn't have a position
--
--  * Disabled and Disconnected screens should coorespond to the command
--    `--output <OutputName> --off`
--  * We would like to keep the Disabled information for later. When the screen
--    state expression is used in a State monad this will allow for disabling
--    secondary screens that are currently enabled.
--  * 

-- | represents the physical layout of those screens which are connected
data ScreenF b = 
    Primary      OutputName Config
  | Secondary    OutputName Config Position b
  | Disabled     OutputName Config b
  | Disconnected OutputName b
  deriving (Show, Functor)

type Screens = Fix ScreenF

data Position =
    RightOf
  | LeftOf 
  | Above  
  | Below  
  | SameAs 
  deriving (Show)

newtype OutputName = OutputName { name :: T.Text } 
  deriving (Show, Eq, Ord, IsString)

data Config = Config
  { rotation :: Rotation
  , mode     :: Mode
  } deriving (Show, Eq, Ord)

newtype Mode = Mode { modeName :: (Natural, Natural) }
  deriving (Show, Eq, Ord)

data Rotation =
    Normal
  | RotateLeft
  | RotateRight
  | Inverted
  deriving (Show, Eq, Ord)

type Cmd = [T.Text]

class ToCmd a where
  buildCmd :: a -> Cmd

instance ToCmd OutputName where
  buildCmd n = ["--output", name n]

instance ToCmd Config where
  buildCmd m = 
       (buildCmd . mode $ m)
    <> (buildCmd . rotation $ m)
    
instance ToCmd Rotation where
  buildCmd x = ["--rotate", rotateOption x]
    where
      rotateOption Normal      = "normal"
      rotateOption RotateLeft  = "left"
      rotateOption RotateRight = "right"
      rotateOption Inverted    = "inverted"

instance ToCmd Mode where
  buildCmd m =
    [ "--mode"
    , (T.pack . show . modeX $ m) <> "x" <> (T.pack . show . modeY $ m)
    ]

modeX = fst . modeName

modeY = snd . modeName

makeCmd :: Screens -> Cmd
makeCmd = para buildCmd'

buildCmd' :: ScreenF (Screens, Cmd) -> Cmd
buildCmd' (Primary n c)                     =         (buildCmd n) <> (buildCmd c) <> ["--primary"]
buildCmd' (Secondary n c p (screens, cmds)) = cmds <> (buildCmd n) <> (buildCmd c) <> [positionArg p, name $ nextOutputName screens]
buildCmd' (Disabled n c (_, cmds))          = cmds <> (buildCmd n) <> (buildCmd c) <> ["--off"]
buildCmd' (Disconnected n (_, cmds))        = cmds <> (buildCmd n)                 <> ["--off"]

positionArg :: Position -> T.Text
positionArg LeftOf  = "--left-of"
positionArg RightOf = "--right-of"
positionArg Above   = "--above"
positionArg Below   = "--below"
positionArg SameAs  = "--same-as"

nextOutputName :: Screens -> OutputName
nextOutputName = cata nextOutputName'

nextOutputName' :: ScreenF OutputName -> OutputName
nextOutputName' (Primary n _)      = n
nextOutputName' (Secondary n _ _ _)  = n
nextOutputName' (Disabled _ _ n)   = n
nextOutputName' (Disconnected _ n) = n

configWithNormalRotation :: Mode -> Config
configWithNormalRotation = Config Normal

data SC = 
    Prim :: Screen
  | Pre :: Screen -> Screen 

instance SemiGroup SC where
   (Prim s) <> (Prim _) = Prim s
   (Pre f)  <> (Pre g)  = Pre $ g . f
   (Prim s) <> (Pre f)  = Prim $ f s
   (Pre f)  <> (Prim s) = Prim $ f s

main = undefined

-- parseXrandr = parseOnly $ parseOutputInfos
-- 
-- parseOutputInfos :: Parser SC
-- parseOutputInfos = ignoreRestOfLine *> many parseOutputInfo

screenText :: Parser SC
screenText = 
      isPrimary
  <|> isSeconday
  <|> isDisabled
  <|> isDisconnected
  
isDisconnected :: Parser SC
isDisconnected = 
  ((Pre . Fix .) <$> Disconnected)
  <$> outputNameText
  <* skipSpace
  <* string "disconnected"
  <* ignoreRestOfLine

isPrimary :: Parser SC
isPrimary = isConnected is

isSeconday :: Parser 

isConnected :: Parser Config -> Parser a -> Parser SC
isConnected config primary =
  ((Prim . Fix .) <$> Primary
  <$> outputNameText
  <* skipSpace
  <* string "connected" 
  <* primary
  <* ignoreRestOfLine
  <*> config

outputNameText :: Parser OutputName
outputNameText = takeTill (==' ')



modeWith :: Parser Bool -> Parser (Mode, Bool)
modeWith p = 
      ((,))
  <*  (many1 $ char ' ') 
  <$> parseMode
  <*  skipSpace
  <*  take 5
  <*> p
  <*  ignoreRestOfLine

preferredMode

preferredModeText :: Parser Bool
preferredModeText = flag $ char '+'

enabledModeText :: Parser Bool
enabledModeText = flag $ char '*'

modeText :: Parser Mode
modeText = 
  (Mode . (,))
  <$> natText 
  <* char 'x' 
  <*> natText

flag :: Parser a -> Parser Bool
flag p = pure True <* p <|> pure False

natText :: Parser Natural
natText = read <$> many1 digit

ignoreRestOfLine :: Parser ()
ignoreRestOfLine = takeTill isEndOfLine *> endOfLine

-- main = readFile "out.txt" >>= either print buildAndRun . parseXrandr . T.pack
-- --   runXrandr []
-- --   >>= 
-- --     either print (buildAndRun >=> print)
-- --     . parseXrandr 
-- 
-- buildAndRun = print . sortOn (status . outputInfo)
-- 
-- runXrandr :: [T.Text] -> IO T.Text
-- runXrandr = shelly . run "xrandr"
-- 
-- asExternal :: (Foldable f, Functor f) => Position -> a -> f a -> Outputs a
-- asExternal p = flip (asDir p) . primary 
-- 
-- asDir :: (Foldable f) => Position -> f a -> Outputs a -> Outputs a
-- asDir d = appEndo . foldMap (Endo . secondary d)
