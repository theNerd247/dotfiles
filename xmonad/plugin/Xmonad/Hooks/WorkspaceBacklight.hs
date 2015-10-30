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
  ,Brightness(..)
  ,setWSBacklight
)
where

import qualified Xmonad.Core       as XC
import qualified Xmonad.Util.ExtensibleState as XS
import qualified Data.Map          as M
import System.Process

type Brightness = Int

type BacklightState i = M.Map i Brightness

type BacklightConf = BacklightState WorkspaceId

-- | Sets the backlight of the screen
setScreenBacklight :: Brightness -> X ()
setScreenBacklight = XC.spawn . ("xbacklight -set " ++) . show

-- | Sets the screen brightness given the workspace id
setWSBacklight :: (Ord i) => i -> X ()
setWSBacklight w = do
      bs <- XS.get :: X (BacklightState i)
      let setbs
        | (Just b) <- M.lookup w bs = setScreenBacklight b
        | otherwise = return ()
      in setbs

-- | Binds the screen's brightness to the current workspace
adjustWSBacklight :: (Ord i) => i -> X()
adjustWSBacklight w = do
  b <- getBrightness
  XS.modify $ M.adjust (\_ -> b) w

enableBacklightControl :: WorkspaceId -> X ()
enableBacklightControl i = XS.modify $ M.insert 50 i

disableBacklightControl :: WorkspaceId -> X ()
disableBacklightControl i = XS.modify $ M.delete i

-- | Gets the current screen's brightness
getBrightness :: X Brightness
getBrightness = XC.io $ readProcess "xbacklight" [] []
