{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (takeWhile, take)
import Numeric.Natural
import Data.Foldable (foldMap)
import Control.Arrow
import Control.Applicative
import Data.Attoparsec.Text
import Data.Monoid
import Data.List (partition, find, sortOn)
import Data.Functor.Foldable
import Shelly (shelly, run, run_)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Control.Monad ((>=>))
import Data.Bifunctor (bimap)

-- | represents the physical layout of those screens which are connected
data ScreensF b = 
    NoScreens
  | Disabled OutputName b
  | Disconnected OutputName b
  | Primary OutputName Config b
  | Secondary OutputName Position Config b
  deriving (Show, Functor)

type Screens = Fix ScreensF

data Position =
    RightOf
  | LeftOf 
  | Above  
  | Below  
  | SameAs 
  deriving (Show)

type OutputName = T.Text

data Config = Config
  { rotation :: Rotation
  , mode     :: Mode
  } deriving (Show, Eq, Ord)

type Mode = (Natural, Natural)

data Rotation =
    Normal
  | RotateLeft
  | RotateRight
  | Inverted
  deriving (Show, Eq, Ord)

type Cmd = [T.Text]

class ToCmd a where
  buildCmd :: a -> Cmd

instance (ToCmd a) => ToCmd (Maybe a) where
  buildCmd = maybe [] buildCmd

instance (Show a, Show b) => ToCmd (a, b) where
  buildCmd (x,y) =
    [ "--mode"
    , (T.pack . show $ x) <> "x" <> (T.pack . show $ y)
    ]

instance ToCmd Output where
  buildCmd Output{ name, status } =
    [ "--output"
    , name 
    ] 
    <> (buildCmd status)

instance ToCmd Config where
  buildCmd m = 
       (buildCmd . mode $ m)
    <> (buildCmd . rotation $ m)
    
instance ToCmd Rotation where
  buildCmd x = ["--rotate", rotateOption x]

rotateOption :: Rotation -> T.Text
rotateOption Normal      = "normal"
rotateOption RotateLeft  = "left"
rotateOption RotateRight = "right"
rotateOption Inverted    = "inverted"

buildCmd' :: (ToCmd a, HasOutput a) => ScreensF (Screens, Cmd) -> Cmd
buildCmd' NoScreens                      = []
buildCmd' Disconnected n (_, cmds)       = cmds <> (buildCmd n) <> ["--off"]
buildCmd' Disabled n     (_, cmds)       = cmds <> (buildCmd n) <> ["--off"]
buildCmd' (Primary o c   (_, cmds))      = cmds <> (buildCmd o) <> (buildCmd c) <> ["--primary"]
buildCmd' (Secondary o p (Fix os, cmds)) = cmds <> (buildCmd o) <> (buildCmd p) <> [positionArg p, output os]

makeCmd :: (ToCmd a, HasOutput a) => Screens -> Cmd
makeCmd = para buildCmd'

positionArg :: Position -> T.Text
positionArg LeftOf  = "--left-of"
positionArg RightOf = "--right-of"
positionArg Above   = "--above"
positionArg Below   = "--below"
positionArg SameAs  = "--same-as"

class HasOutput a where
  output :: a -> OutputName

instance HasOutput Output where
  output = name

instance (HasOutput a) => HasOutput (ScreensF a b) where
  output (Primary o) = output o
  output (Secondary _ o _) = output o

primary :: a -> Outputs a
primary = Fix . Primary

secondary :: Position -> a -> Outputs a -> Outputs a
secondary p x = Fix . (Secondary p x)

leftOf = secondary LeftOf
rightOf = secondary RightOf

data OutputInfo = OutputInfo
  { isPrimary  :: Bool
  , outputInfo :: Output
  } deriving (Show)

instance HasOutput OutputInfo where
  output = output . outputInfo

parseXrandr = parseOnly $ parseOutputInfos

parseOutputInfos :: Parser (Outputs Output)
parseOutputInfos = ignoreRestOfLine *> many parseOutputInfo

parseOutput :: Parser (Outputs Output) 
parseOutput = 
  (fmap . Output) 
  <$> parseOutputName 
  <*  skipSpace
  <*> parseStatus

parseOutputName :: Parser OutputName
parseOutputName = takeTill (==' ')

parseStatus :: Parser (ScreensF Status a)
parseStatus = isDisconnected <|> isConnected

isDisconnected :: Parser (ScreensF Status a)
isDisconnected = 
  pure (False, Disconnected) 
  <* string "disconnected"
  <* ignoreRestOfLine

isConnected :: Parser (Bool, Status)
isConnected = 
  pure (,)
  <*  string "connected" 
  <* skipSpace
  <*> parsePrimary 
  <*  skipSpace
  <*  ignoreRestOfLine
  <*> parseModes

parsePrimary :: Parser Bool
parsePrimary = flag $ string "primary"

parseModes :: Parser Status
parseModes = 
  (maybe Disabled (defaultConfig . fst) . find snd) 
  <$> many parseModeAndEnabled

defaultConfig :: Mode -> Status
defaultConfig = Enabled . Config Normal

parseModeAndEnabled :: Parser (Mode, Bool)
parseModeAndEnabled = 
      pure (,)
  <*  (many1 $ char ' ') 
  <*> parseMode
  <*  skipSpace
  <*  take 5
  <*> ((||) <$> (flag $ char '*') <*> (flag $ char '+'))
  <*  ignoreRestOfLine

parseMode :: Parser Mode
parseMode = 
  (,)
  <$> parseNat 
  <* char 'x' 
  <*> parseNat

flag :: Parser a -> Parser Bool
flag p = pure True <* p <|> pure False

parseNat :: Parser Natural
parseNat = read <$> many1 digit

ignoreRestOfLine :: Parser ()
ignoreRestOfLine = takeTill isEndOfLine *> endOfLine

main = readFile "out.txt" >>= either print buildAndRun . parseXrandr . T.pack
--   runXrandr []
--   >>= 
--     either print (buildAndRun >=> print)
--     . parseXrandr 

buildAndRun = print . sortOn (status . outputInfo)

runXrandr :: [T.Text] -> IO T.Text
runXrandr = shelly . run "xrandr"

asExternal :: (Foldable f, Functor f) => Position -> a -> f a -> Outputs a
asExternal p = flip (asDir p) . primary 

asDir :: (Foldable f) => Position -> f a -> Outputs a -> Outputs a
asDir d = appEndo . foldMap (Endo . secondary d)
