{-# LANGUAGE FlexibleContexts, PatternGuards #-}

import Control.Applicative ((<$>),(<*>))
import XMonad
import XMonad.Actions.WindowBringer
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeWindows
import XMonad.Util.Font
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Util.Loggers
import qualified XMonad.Hooks.EwmhDesktops as EWMH
import qualified XMonad.StackSet as SS
import qualified XMonad.Util.EZConfig as EZ

{-import XMonad.Actions.WorkspaceBacklight-}

modmask = mod4Mask
home = "/home/noah/"

-- default applications
webbrowser = "google-chrome-stable"

-- remove the ncmpcpp toggle command as it's currently broken
toggleSound = "amixer set Master toggle"
term = "termite"
volUp = "amixer set Master 5%+"
volDown = "amixer set Master 5%-"
volToggle = "amixer set Master toggle"
bckLightDown = "xbacklight -dec 5"
bckLightUp = "xbacklight -inc 5"

-- custom keyboard mappings
customkeys :: [(String,X ())]
customkeys =
  [ ("M-v",                      spawn volUp)
  , ("M-S-v"                   , spawn volDown)
  , ("M-c"                     , spawn webbrowser)
  , ("M-t"                     , spawn term)
  , ("M-S-t"                   , withFocused $ windows . SS.sink)
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
  [ isFullscreen -?> doFullFloat --make fullscreen windows (as when watching a video) floating instead of tiled
  , isDialog     -?> doCenterFloat
  ]

-- X event hooks
myEventHooks = 
  EWMH.fullscreenEventHook
  <+> (handleEventHook def)

myXmobar = statusBar myXmobarCmd myPP toggleStrutsKey
  where
    myXmobarCmd = "xmobar -o -t '%StdinReader%' -f Monospace -c '[Run StdinReader]'"
    myPP = xmobarPP
      { ppExtras = 
          [ onRight logTitle
          , onCenter $ 
              date "%a %Y-%m-%d %r"
              `mappend` logSp 3
              `mappend` batt
          ]
      , ppOrder = \(w:_:_:n:d:[]) -> [ws w,d,n]
      , ppSep = "   "
      }
    ws w = take 110 $ w ++ (repeat ' ')
    onCenter = fixedWidthL AlignCenter " " 90
    onRight = fixedWidthL AlignRight " " 100
    toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b)
    batt = 
      logCmd "acpi | sed -r 's/.*?: .*, (.*)%.*/\\1/'"
        >>= return . fmap battColor
      where
        battColor c
          | b < 33 = xmobarColor "green" "red" ("***"++c++"***")
          | b <= 66 = xmobarColor "yellow" "" ("**"++c++"**")
          | b <= 100 = xmobarColor "green" "" c
          where
            b = read c :: Int

main = xmonad =<< myXmobar config
  where
    config = def 
      { manageHook = myManageHooks
      , handleEventHook = myEventHooks
      , layoutHook = myLayoutHooks
      , modMask = modmask
      , terminal = "xterm"
      , normalBorderColor = unfocusBorder
      , focusedBorderColor = focusBorder
      }
      `EZ.additionalKeysP` customkeys
