import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run (spawnPipe)
import XMonad.Layout.NoBorders
import XMonad.Layout.Circle
import XMonad.Layout.Fullscreen
import XMonad.Layout.Grid
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Tabbed
import XMonad.Layout.Minimize
import XMonad.Hooks.ManageHelpers
import XMonad.Actions.WindowBringer
import XMonad.Actions.CycleWS
import XMonad.Actions.MouseGestures
import XMonad.Actions.WithAll
import XMonad.Actions.OnScreen
import XMonad.Hooks.FadeWindows
import System.IO
import Data.Monoid (mconcat)

import qualified Data.Map as M
import qualified XMonad.StackSet as SS
import qualified XMonad.Util.EZConfig as EZ
import Control.Applicative ((<$>),(<*>))
import Data.Maybe (listToMaybe, fromJust)

modmask = mod4Mask
home = "/home/noah/"

-- default applications
term = "urxvt"
inTerm :: String -> String
inTerm = ("exec urxvt -e " ++)
webbrowser = "firefox"

-- keyboard applications
scripts = home ++ ".dotfiles/xmonad/scripts/"

-- remove the ncmpcpp toggle command as it's currently broken
toggleSound = "amixer set Master toggle"
-- remove the ncmpcpp toggle command as it's currently broken
toggleMusic = ""
volUp = "amixer set Master 5%+"
volDown = "amixer set Master 5%-"
musicPlayer =  scripts ++ "openNCMPCPP"
mail = scripts ++ "openMail"
lockPc = "xscreensaver-command -lock"
network = inTerm "wicd-curses"
ranger = inTerm "ranger"
bckLightDown = "xbacklight -set 20"
bckLightUp = "xbacklight -set 50"

-- TODO: make this move the given program to the desired workspace
-- executes the given shell command then goes to the given workspace
newProg :: String -> WorkspaceId -> X ()
newProg c i = spawn c >> (windows $ vs <*> getScreenId i)
  where
    vs ws Nothing = ws
    vs ws (Just s) = viewOnScreen s i ws

-- gets the screen of the given workspace
getScreenId :: WorkspaceId -> WindowSet -> Maybe ScreenId
getScreenId s ws = listToMaybe [sid | SS.Screen i sid _ <- SS.screens ws, (SS.tag i) == s]

-- custom keyboard mappings
customkeys :: [(String,X ())]
customkeys =
  [ ("M-a", spawn toggleSound)
  , ("M-S-a", spawn toggleMusic)
  , ("M-v", spawn volUp)
  , ("M-S-v", spawn volDown)
  , ("M-m", spawn musicPlayer)
  , ("M-S-m", newProg mail "office")
  , ("M-c", spawn webbrowser)
  , ("M-t", spawn term)
  , ("M-S-t", withFocused $ windows . SS.sink)
  , ("M-S-l", shiftTo Next AnyWS)
  , ("M-S-h", shiftTo Prev AnyWS)
  , ("M-g", gotoMenu)
  , ("M-b", bringMenu)
  , ("M-M1-l", spawn lockPc)
  , ("M-n", spawn network)
  , ("M-f", spawn ranger)
  , ("M-<F2>", spawn bckLightDown)
  , ("M-<F3>", spawn bckLightUp)
  ]

{-hideAllWindows = hide SS.allWindows-}

gestures = M.fromList 
[([D], (\w -> withAll hide))
,([U], (\w -> withAll reveal))
]

-- button4 is scrollup
customMouse = 
  [((modmask,button4), (\w -> windows SS.focusUp)) 
  ,((modmask,button5), (\w -> windows SS.focusDown))
  ,((modmask .|. shiftMask,button1), mouseGesture gestures)
  ]

-- border colors
focusBorder = "#a0a0a0"
unfocusBorder = "#303030"

-- default Tall config 
tiled = Tall 
{tallNMaster = nm
,tallRatioIncrement = inc
,tallRatio = rt}
  where 
    nm = 1
    inc = 3/100
    rt = 1/2

-- tabbed layout config
myTabbed = tabbedBottom shrinkText tabCfg
  where tabCfg = defaultTheme

-- layouts
layouts = myTabbed
  ||| tiled
  ||| Grid
  ||| Circle 
  ||| Full
  ||| Mirror tiled

-- layout hooks 
myLayoutHooks = avoidStruts 
$ smartBorders
$ fullscreenFull 
$ fullscreenFloat
$ layouts

-- workspace details
myWorkspaces = (custom ++) $ show <$> drop n [1..9]
  where 
    custom = ["web","term","office","media"]
    n = length custom

-- hooks to perform when a window opens
myManageHooks = 
  manageDocks
  <+> fullscreenManageHook 
  <+> helpers
  <+> windowToWorkSpace
  {-<+> fadeHooks-}
  <+> manageHook defaultConfig

windowToWorkSpace = composeAll
[ 
className =? "URxvt" --> doShift "term"
,className =? "Firefox" --> doShift "web"
,className =? "Gimp" --> doShift "media"
,className =? "Evince" --> doShift "office"
]

{-fadeHooks = composeAll -}
{-[-}
{-className =? "URxvt" --> fadeTo 0-}
{-]-}

helpers = composeOne
[isFullscreen -?> doFullFloat]

-- X event hooks
myEventHooks = 
  fullscreenEventHook
  <+> (handleEventHook defaultConfig)

main = do
  spawnPipe "/home/noah/.xmonad/autostart.sh"
  xmproc <- spawnPipe "/usr/bin/xmobar -o /home/noah/.xmobarcc"
  xmonad $ defaultConfig
  { manageHook = myManageHooks
  , handleEventHook = myEventHooks
  , layoutHook = myLayoutHooks
  , modMask = modmask
  , terminal = term
  , normalBorderColor = unfocusBorder
  , focusedBorderColor = focusBorder
  , workspaces = myWorkspaces
  , logHook = dynamicLogWithPP xmobarPP
  { ppOutput = hPutStrLn xmproc
  , ppTitle = xmobarColor "green" "" . shorten 50
  }
  }
  `EZ.additionalKeysP` customkeys
  `EZ.additionalMouseBindings` customMouse
