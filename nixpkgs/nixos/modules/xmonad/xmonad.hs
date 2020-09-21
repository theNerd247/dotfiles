{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Control.Applicative ((<$>),(<*>))
import XMonad
import XMonad.Actions.WindowBringer
import XMonad.Hooks.FadeWindows
import XMonad.Util.Font
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Util.Loggers
import Control.Monad.IO.Class
import qualified XMonad.Hooks.EwmhDesktops as EWMH
import qualified XMonad.StackSet as SS
import qualified XMonad.Util.ExtensibleState as XS
import qualified XMonad.Util.EZConfig as EZ

{-import XMonad.Actions.WorkspaceBacklight-}

modmask = mod4Mask

-- default applications
webbrowser = "firefox"

-- remove the ncmpcpp toggle command as it's currently broken
term = "termite"
pdfviewer = "zathura"

-- use amixer or pactl?
data VolControl = Amixer | Pactl deriving (Eq, Ord, Show)

volControl = Pactl

volUp = 
  case volControl of
    Amixer -> "amixer set Master 5%+"
    Pactl -> "pactl set-sink-volume @DEFAULT_SINK@ +5%"

volDown = 
  case volControl of
    Amixer -> "amixer set Master 5%-"
    Pactl -> "pactl set-sink-volume @DEFAULT_SINK@ -5%"

volToggle = 
  case volControl of 
    Amixer -> "amixer set Master toggle"
    Pactl -> "pactl set-sink-mute @DEFAULT_SINK@ toggle"

bckLightDown = "xbacklight -dec 5"
bckLightUp = "xbacklight -inc 5"

-- custom keyboard mappings
customkeys :: [(String,X ())]
customkeys =
  [ ("M-v",                      spawn volUp)
  , ("M-S-v"                   , spawn volDown)
  , ("M-a"                     , spawn volToggle)
  , ("M-c"                     , spawn webbrowser)
  , ("M-t"                     , spawn term)
  , ("M-S-t"                   , withFocused $ windows . SS.sink)
  , ("M-z"                     , spawn pdfviewer)
  , ("<XF86MonBrightnessUp>"   , spawn bckLightUp)
  , ("<XF86MonBrightnessDown>" , spawn bckLightDown)
  , ("<XF86AudioRaiseVolume>"  , spawn volUp)
  , ("<XF86AudioLowerVolume>"  , spawn volDown)
  , ("<XF86AudioMute>"         , spawn volToggle)
  ]

-- border colors
focusBorder = "#a0a0a0"
unfocusBorder = "#303030"

-- default Tall config 
tiled = Tall 
  { tallNMaster = nm
  , tallRatioIncrement = inc
  , tallRatio = rt
  }
  where 
    nm = 1
    inc = 3/100
    rt = 1/2

-- tabbed layout config
myTabbed = tabbedBottom shrinkText tabCfg
  where tabCfg = def

-- layouts
layouts = myTabbed
  ||| tiled
  ||| Grid
  ||| Full

-- layout hooks 
myLayoutHooks = 
    smartBorders
  $ layouts

-- hooks to perform when a window opens
myManageHooks = helpers <+> manageHook def

helpers = composeOne
  [ isFullscreen -?> doFullFloat 
  -- ^ make fullscreen windows (as when watching a video) floating instead of tiled
  , isDialog     -?> doCenterFloat
  ]

-- X event hooks
myEventHooks = 
  EWMH.fullscreenEventHook
  <+> (handleEventHook def)

main = xmonad config
  where
    config = def 
      { manageHook = myManageHooks
      , handleEventHook = myEventHooks
      , layoutHook = myLayoutHooks
      , modMask = modmask
      , terminal = "termite"
      , normalBorderColor = unfocusBorder
      , focusedBorderColor = focusBorder
      }
      `EZ.additionalKeysP` customkeys


