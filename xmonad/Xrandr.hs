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
import Data.List (partition, find)
import Data.Functor.Foldable
import Shelly (shelly, run, run_)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Control.Monad ((>=>))
import Data.Bifunctor (bimap)

type Outputs a = Fix (OutputsF a)

data OutputsF a b = 
    Primary a
  | Secondary Position a b
  deriving (Show, Functor)

data Position =
    RightOf
  | LeftOf 
  | Above  
  | Below  
  | SameAs 
  deriving (Show)

data Output = Output
  { name     :: OutputName
  , status   :: Status
  } deriving (Show)

type OutputName = T.Text

data Status = 
    Disconnected
  | Disabled
  | Enabled Config
  deriving (Show)

data Config = Config
  { rotation :: Rotation
  , mode     :: Mode
  } deriving (Show)

type Mode = (Natural, Natural)

data Rotation =
    Normal
  | RotateLeft
  | RotateRight
  | Inverted
  deriving (Show)

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

instance ToCmd Status where
  buildCmd (Enabled m) = buildCmd m
  buildCmd _ = ["--off"]

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

instance (HasOutput a) => HasOutput (OutputsF a b) where
  output (Primary o) = output o
  output (Secondary _ o _) = output o

primary :: a -> Outputs a
primary = Fix . Primary

secondary :: Position -> a -> Outputs a -> Outputs a
secondary p x = Fix . (Secondary p x)

leftOf = secondary LeftOf
rightOf = secondary RightOf

buildCmd' :: (ToCmd a, HasOutput a) => OutputsF a (Outputs a, Cmd) -> Cmd
buildCmd' (Primary o) = buildCmd o <> ["--primary"]
buildCmd' (Secondary p o (Fix os, cmds)) = 
  cmds 
  <> (buildCmd o) 
  <> [positionArg p]
  <> [output os]

makeCmd :: (ToCmd a, HasOutput a) => Outputs a -> Cmd
makeCmd = para buildCmd'

data OutputInfo = OutputInfo
  { isPrimary  :: Bool
  , outputInfo :: Output
  } deriving (Show)

instance HasOutput OutputInfo where
  output = output . outputInfo

main = readFile "out.txt" >>= either print buildAndRun . parseXrandr . T.pack
--   runXrandr []
--   >>= 
--     either print (buildAndRun >=> print)
--     . parseXrandr 

buildAndRun = 
    print . inlinedOutputs (asExternal LeftOf)

runXrandr :: [T.Text] -> IO T.Text
runXrandr = shelly . run "xrandr"

parseXrandr = parseOnly $ parseOutputInfos

parseOutputInfos :: Parser [OutputInfo]
parseOutputInfos = ignoreRestOfLine *> many parseOutputInfo

parseOutputInfo :: Parser OutputInfo
parseOutputInfo = (uncurry OutputInfo) <$> parseOutput 

parseOutput :: Parser (Bool, Output)
parseOutput = 
  (fmap . Output) 
  <$> parseOutputName 
  <*  skipSpace
  <*> parseStatus

parseOutputName :: Parser OutputName
parseOutputName = takeTill (==' ')

parseStatus :: Parser (Bool, Status)
parseStatus = isDisconnected <|> isConnected

isDisconnected :: Parser (Bool, Status)
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

inlinedOutputs f = 
  maybe mempty makeCmd 
  . fmap (uncurry f) 
  . getPrimary 

getPrimary :: [OutputInfo] -> Maybe (Output, [Output])
getPrimary xs = do
  let (ps, nps) = bimap (fmap outputInfo) (fmap outputInfo) $ partition isPrimary xs
  h <- listToMaybe ps
  return (h, nps ++ (tail ps))

asExternal :: (Foldable f, Functor f) => Position -> a -> f a -> Outputs a
asExternal p = flip (asDir p) . primary 

asDir :: (Foldable f) => Position -> f a -> Outputs a -> Outputs a
asDir d = appEndo . foldMap (Endo . secondary d)
