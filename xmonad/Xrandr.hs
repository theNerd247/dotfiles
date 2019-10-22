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
import Shelly (shelly, run)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T

-- newtype Fix f = Fix { unFix :: f(Fix f) }
-- 
-- cata :: (Functor f) => (f a -> a) -> Fix f -> a
-- cata alg = alg . fmap (cata alg) . unFix
-- 
-- ana :: (Functor f) => (a -> f a) -> a -> Fix f
-- ana coalg = Fix . fmap (ana coalg) . coalg
-- 
-- hylo :: (Functor f) => (f b -> b) -> (a -> f a) -> a -> b
-- hylo alg coalg = alg . fmap (hylo alg coalg) . coalg 
-- 
-- para :: (Functor f) => (f (Fix f, a) -> a) -> Fix f -> a
-- para palg = palg . fmap ((,) <*> para palg) . unFix

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

type OutputName = String

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

type Cmd = String

class ToCmd a where
  buildCmd :: a -> Cmd

instance (ToCmd a) => ToCmd (Maybe a) where
  buildCmd = maybe "" buildCmd

instance ToCmd Mode where
  buildCmd Disabled = "--off"
  buildCmd (ModeName m) = "--mode " <> buildCmd m

instance (ToCmd a, ToCmd b) => ToCmd (a,b) where
  buildCmd (x,y) = buildCmd x <> "x" <> buildCmd y

instance ToCmd Natural where
  buildCmd = show

instance ToCmd Char where
  buildCmd = return

instance (ToCmd a) => ToCmd [a] where
  buildCmd = foldMap buildCmd

instance ToCmd Position where
  buildCmd LeftOf  = "--left-of"
  buildCmd RightOf = "--right-of"
  buildCmd Above   = "--above"
  buildCmd Below   = "--below"
  buildCmd SameAs  = "--same-as"

instance ToCmd Rotation where
  buildCmd Normal      = "normal"
  buildCmd RotateLeft  = "left"
  buildCmd RotateRight = "right"
  buildCmd Inverted    = "inverted"

instance ToCmd Output where
  buildCmd Output{ name, mode, rotation} =
    "--output " <> name
    <> " " <> (buildCmd mode)
    <> " --rotate " <> (buildCmd rotation)

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
buildCmd' (Primary o) = (buildCmd o) <> " --primary"
buildCmd' (Secondary p o (Fix os, cmds)) = 
  cmds 
  <> " " <> (buildCmd o) 
  <> " " <> (buildCmd p)
  <> " " <> (output os)

data OutputInfo = OutputInfo
  { outputInfoName :: OutputName
  , isConnected    :: Bool
  , isPrimary      :: Bool
  , modeNames      :: [ModeName]
  , isEnabled      :: Bool
  } deriving (Show)

instance HasOutput OutputInfo where
  output = outputInfoName

main = runXrandr >>= return . parseXrandr

runXrandr = -- T.pack <$> readFile "out.txt"
  shelly $ run "xrandr" []

parseXrandr = parseOnly $ parseOutputInfos <* endOfInput

parseOutputInfos :: Parser [OutputInfo]
parseOutputInfos = string "Screen" *> ignoreRestOfLine *> many parseOutputInfo

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
  <**> (pure uncurry)
  <*>  parseModeNames

parseOutputName :: Parser OutputName
parseOutputName = T.unpack <$> takeTill (==' ')

parseConnected :: Parser Bool
parseConnected = 
      (pure True  <* string "connected") 
  <|> (pure False <* string "disconnected")

parsePrimary :: Parser Bool
parsePrimary = flag $ string "primary"

parseModeNames :: Parser ([ModeName], Bool)
parseModeNames = anyEnabledModes <$> many parseModeNameAndEnabled

anyEnabledModes :: [(ModeName, Bool)] -> ([ModeName], Bool)
anyEnabledModes = (fmap fst) &&& (getAny . foldMap (Any. snd))

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

filterIsConnected :: [OutputInfo] -> [OutputInfo]
filterIsConnected = filter isConnected

getPrimary :: [OutputInfo] -> Maybe (OutputInfo, [OutputInfo])
getPrimary xs = do
  let (ps, nps) = partition isPrimary xs
  h <- listToMaybe ps
  return (h, nps ++ (tail ps))
