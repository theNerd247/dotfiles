{-# LANGUAGE DeriveDataTypeable #-}

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
import qualified Xmonad.Core       as XC
import qualified Xmonad.Util.ExtensibleState as XE
import qualified Data.Map          as M
import Control.Monad.State (StateT(..), modify, get)

type Brightness = Int

type Backlights i = M.Map i Brightness

-- | Sets the backlight of the screen
setScreenBacklight :: Brightness -> X ()
setScreenBacklight = XC.spawn . ("xbacklight -set " ++) . show

-- | Sets the screen brightness given the workspace id
setWSBacklight :: (Ord i) => i -> WSBacklightS i ()
setWSBacklight w = get >>= fmap setbl
  where
    setbl bs
      | (Just b) <- M.lookup w bs = setScreenBacklight b
      | otherwise = return ()

-- | Adjusts the stored backlight and sets the screens backlight
adjustWSBacklight :: (Ord i) => i -> Brightness -> WSBacklightS i ()
adjustWSBacklight w b = 
  modify (M.adjust (\_ -> b) w) 
  setWSBacklight w

-- | Runs an action in the WSBacklightS monad by fetching the config from the X
-- state and stores the resulting config in the X state

runWSBacklightAction :: WSBacklightS i () -> X ()
runWSBacklightAction f = do 
  conf <- (XS.get :: X (Backlights i) )
  s <- execStateT f conf
  put s

enableBacklightControl :: WorkspaceId -> X ()
enableBacklightControl i = runWSBacklightAction $ modify $ M.insert 50 i

disableBacklightControl :: WorkspaceId -> X ()
disableBacklightControl i = runWSBacklightAction $ modify $ M.delete i
