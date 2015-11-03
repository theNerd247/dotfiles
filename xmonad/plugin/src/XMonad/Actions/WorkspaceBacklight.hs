{-# LANGUAGE DeriveDataTypeable #-}
{-#LANGUAGE TypeSynonymInstances #-}
{-#LANGUAGE FlexibleInstances #-}

{-|
Module      : WorkspaceBacklight
Description : Set the screen backlight for each individual workspace using xbacklight
Copyright   : (c) theNerd247 2015
License     : GPL-2
Maintainer  : noah.harvey247@gmail.com
Stability   : experimental
Portability : POSIX

-}

module XMonad.Actions.WorkspaceBacklight
(
   Brightness(..)
  ,BacklightConf(..)
  ,adjustWSBacklight
  ,setBacklight
)
where

import Control.Monad.Trans.Maybe
import XMonad.Core
import System.Process
import XMonad

import qualified Data.Map as M
import qualified XMonad.Util.ExtensibleState as XS
import qualified XMonad.StackSet as XSS
import qualified XMonad.Util.EZConfig as XEZ

type Brightness = Int

type BacklightState s i = M.Map (s,i) Brightness

type BacklightConf = BacklightState ScreenId WorkspaceId

instance (Typeable s, Typeable i) => ExtensionClass (BacklightState s i)

-- | Sets the given backlight for the given screen
setScreenBacklight :: (Show s) => s ->  Brightness -> X ()
setScreenBacklight s b = spawn $ 
  "xbacklight " 
  ++ "-display " ++ (show s)
  ++ "-set " ++ (show b)

-- | returns the current screen and its workspace id (if the workspace id
-- doesn't exist it returns nothing
getScreenAndWSId :: (Eq sid) => XSS.StackSet i l a sid sd -> X (Maybe (sid,i))
getScreenAndWSId ss =
    let 
      s = XSS.screen . XSS.current $ ss
      w = XSS.lookupWorkspace s $ ss
    in maybe (return Nothing) (return . (Just . (,) s)) w

-- | Sets the screen brightness for the current workspace
setBacklight :: X ()
setBacklight = do
  -- get the current screen
  sw <- withWindowSet getScreenAndWSId
  -- get our backlight config
  bsConf <- XS.get :: X BacklightConf
  maybe
    (return ())
    (\(s,b) -> setScreenBacklight s b)
    $ sw 
      >>= \k@(s,_) -> M.lookup k bsConf
      >>= \b -> return (s,b)

-- | Binds the screen's brightness to the current workspace. Bind the keys that
-- should increase the backlight value to this function.
adjustWSBacklight :: Brightness -> X()
adjustWSBacklight db = do
  sw <- withWindowSet getScreenAndWSId
  b <- getBrightness
  maybe 
    (return ()) 
    (\k -> XS.modify (M.adjust (\_ -> db+b) k))
    $ sw

-- | Gets the current screen's brightness
getBrightness :: X Brightness
getBrightness = io $ readProcess "xbacklight" [] [] >>= return . read

enableScreenBrightness :: ScreenId -> [(WorkspaceId,Brightness)] -> BacklightConf
enableScreenBrightness sid = M.fromList . fmap (\(w,b) -> ((sid,w),b))

-- | enables backlight control
enableWSBrightnessControl :: BacklightConf -> X ()
enableWSBrightnessControl = XS.put 

-- | configures the workspace switching keybindings and the FX86 buttons to
-- enable dynamic control of the screen brightness
enableWSKeys :: XConfig l -> [(ButtonMask,KeySym)] -> XConfig l
enableWSKeys xconf ks =
  let
    newKeys = flip $ foldr adjustKey
    adjustKey = M.adjust $ flip (>>) setBacklight
  in
    xconf {keys = newKeys ks . keys xconf}
