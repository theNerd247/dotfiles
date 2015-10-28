{-|
Module      : WorkspaceBacklight
Description : Set the screen backlight for each individual workspace using xbacklight
Copyright   : (c) theNerd247 2015
License     : GPL-2
Maintainer  : noah.harvey247@gmail.com
Stability   : experimental
Portability : POSIX

-}

module XMonad.Hooks.WorkspaceBacklight
(
   Backlights(..)
  ,Brightness(..)
  ,setWSBacklight
)
where

import qualified Xmonad.Operations as XO
import qualified Xmonad.Stackset   as XS
import qualified Xmonad.Core       as XC
import qualified Data.Map          as M

type Brightness = Int

type Backlights i = M.Map i Brightness 

type WSBacklightT i a = StateT (Backlights i) X a

-- | Sets the backlight of the screen
setScreenBacklight :: Brightness -> X ()
setScreenBacklight = XC.spawn . ("xbacklight -set " ++) . show

-- | Sets the screen brightness given the workspace id
setWSBacklight :: (Ord i) => i -> WSBacklightT i ()
setWSBacklight w = get >>= fmap setbl
  where
    setbl bs
      | (Just b) <- M.lookup w bs = setScreenBacklight b
      | otherwise = return ()

-- | Adjusts the stored backlight and sets the screens backlight
adjustWSBacklight :: (Ord i) => i -> Brightness -> WSBacklightT i ()
adjustWSBacklight w b = modify (M.adjust (\_ -> b) w) >>= setWSBacklight w

runBacklight
