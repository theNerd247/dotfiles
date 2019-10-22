{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}


module Xrandr where

import Numeric.Natural
import Data.Foldable (foldMap)

newtype Fix f = Fix { unFix :: f(Fix f) }

cata :: (Functor f) => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

ana :: (Functor f) => (a -> f a) -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg

hylo :: (Functor f) => (f b -> b) -> (a -> f a) -> a -> b
hylo alg coalg = alg . fmap (hylo alg coalg) . coalg 

para :: (Functor f) => (f (Fix f, a) -> a) -> Fix f -> a
para palg = palg . fmap ((,) <*> para palg) . unFix

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
  | ModeName (Natural, Natural)
  deriving (Show)

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

buildCmd' :: (ToCmd a, HasOutput a) => OutputsF a (Outputs a, Cmd) -> Cmd
buildCmd' (Primary o) = (buildCmd o) <> " --primary"
buildCmd' (Secondary p o (Fix os, cmds)) = 
  cmds 
  <> " " <> (buildCmd o) 
  <> " " <> (buildCmd p)
  <> " " <> (output os)

testp = Output "eDP1" (ModeName (1920, 1080)) Normal

tests = Output "DP1" (ModeName (1360, 768)) Normal

test = secondary LeftOf tests $ primary testp
