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
import Data.List (partition)
import Data.Functor.Foldable
import Shelly (shelly, run, run_)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Control.Monad ((>=>))

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
  , mode     :: Mode
  , rotation :: Rotation
  } deriving (Show)

type OutputName = T.Text

data Mode = 
    Disabled
  | ModeName ModeName
  deriving (Show)

type ModeName = (Natural, Natural)

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

instance ToCmd Mode where
  buildCmd Disabled = pure "--off"
  buildCmd (ModeName m) = ["--mode", asXrandrMode m]
    where
      asXrandrMode :: ModeName -> T.Text
      asXrandrMode (x,y) = (T.pack . show $ x) <> "x" <> (T.pack . show $ y)

instance (ToCmd a) => ToCmd [a] where
  buildCmd = foldMap buildCmd

instance ToCmd Position where
  buildCmd LeftOf  = pure "--left-of"
  buildCmd RightOf = pure "--right-of"
  buildCmd Above   = pure "--above"
  buildCmd Below   = pure "--below"
  buildCmd SameAs  = pure "--same-as"

instance ToCmd Rotation where
  buildCmd Normal      = pure "normal"
  buildCmd RotateLeft  = pure "left"
  buildCmd RotateRight = pure "right"
  buildCmd Inverted    = pure "inverted"

instance ToCmd Output where
  buildCmd Output{ name, mode, rotation} =
    [ "--output"
    , name 
    , "--rotate"
    ] 
    <> (buildCmd rotation)
    <> (buildCmd mode)

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
  <> (buildCmd p)
  <> [output os]

makeCmd :: (ToCmd a, HasOutput a) => Outputs a -> Cmd
makeCmd = para buildCmd'

data OutputInfo = OutputInfo
  { outputInfoName :: OutputName
  , isConnected    :: Bool
  , isPrimary      :: Bool
  , modeNames      :: [(ModeName, Bool)]
  } deriving (Show)

instance HasOutput OutputInfo where
  output = outputInfoName

main = 
  runXrandr []
  >>= 
    either print (buildAndRun >=> print)
    . parseXrandr 

buildAndRun = 
    runXrandr . inlinedOutputs (asExternal LeftOf)

runXrandr = shelly . run "xrandr"

parseXrandr = parseOnly $ parseOutputInfos

parseOutputInfos :: Parser [OutputInfo]
parseOutputInfos = ignoreRestOfLine *> many parseOutputInfo

parseOutputInfo :: Parser OutputInfo
parseOutputInfo = 
       OutputInfo 
  <$>  parseOutputName 
  <*   skipSpace
  <*>  parseConnected
  <*   skipSpace
  <*>  parsePrimary 
  <*   skipSpace
  <*   ignoreRestOfLine
  <*>  parseModeNames

parseOutputName :: Parser OutputName
parseOutputName = takeTill (==' ')

parseConnected :: Parser Bool
parseConnected = 
      (pure True  <* string "connected") 
  <|> (pure False <* string "disconnected")

parsePrimary :: Parser Bool
parsePrimary = flag $ string "primary"

parseModeNames :: Parser [(ModeName, Bool)]
parseModeNames = many parseModeNameAndEnabled

parseModeNameAndEnabled :: Parser (ModeName, Bool)
parseModeNameAndEnabled = 
     pure (,)
  <* (many1 $ char ' ') 
  <*> parseModeName
  <* skipSpace
  <* take 5
  <*> (flag $ char '*')
  <* ignoreRestOfLine

parseModeName :: Parser ModeName
parseModeName = 
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
  . filterIsConnected

getConnectedAndPrmiary = getPrimary . filterIsConnected

filterIsConnected :: [OutputInfo] -> [OutputInfo]
filterIsConnected = filter isConnected

getPrimary :: [OutputInfo] -> Maybe (OutputInfo, [OutputInfo])
getPrimary xs = do
  let (ps, nps) = partition isPrimary xs
  h <- listToMaybe ps
  return (h, nps ++ (tail ps))

asExternal :: (Foldable f, Functor f) => Position -> OutputInfo -> f OutputInfo -> Outputs Output
asExternal p = flip (asDir p . (fmap $ toOutput Normal)) . primary . (toOutput Normal) 

asDir :: (Foldable f) => Position -> f a -> Outputs a -> Outputs a
asDir d = appEndo . foldMap (Endo . secondary d)

toOutput :: Rotation -> OutputInfo -> Output
toOutput r o = Output
  { name = output o
  , mode = mainMode (modeNames o)
  , rotation = r
  }

mainMode :: [(ModeName, Bool)] -> Mode
mainMode xs = defaultMode $ filter snd xs <|> xs

defaultMode :: [(ModeName, Bool)] -> Mode
defaultMode = maybe Disabled (ModeName . fst) . listToMaybe
