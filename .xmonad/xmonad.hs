import XMonad
import XMonad.Core

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook

import XMonad.Layout
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.LayoutHints
import XMonad.Layout.PerWorkspace
import XMonad.Layout.HintedTile
import XMonad.Layout.Grid
import XMonad.Layout.StackTile
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.IM

import XMonad.Actions.UpdatePointer
--import XMonad.Actions.Promote
--import XMonad.Actions.Warp

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Man

import XMonad.Util.Run
import XMonad.Util.EZConfig 
import System.IO

import Data.Ratio ((%))
import Data.List
import qualified Data.Map as M
--import GHC.IOBase (Handle)
import Graphics.X11.Xlib

imLayout = smartBorders $ IM (1%5)
                          (Or (Title "Buddy List")
                          (And (Resource "main") (ClassName "psi")))

--vfocusFollowsMouse :: Bool
--vfocusFollowMouse = False

main = do
    myStatusBarPipe <- spawnPipe myStatusBar
    conkyBar <- spawnPipe myConkyBar
    --xmproc <- spawnPipe "xmobar"
    xmonad $ myUrgencyHook $ defaultConfig
        { manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
        , normalBorderColor = myInactiveBorderColor
        , focusedBorderColor = myActiveBorderColor
        , terminal = "gnome-terminal"
        , layoutHook = avoidStruts $ myLayoutHook 
        , logHook = dynamicLogWithPP $ myDzenPP myStatusBarPipe
        , modMask = mod4Mask
        , keys = myKeys
        , workspaces = myWorkspaces
        } 

-- Paths
myBitmapsPath = "/home/gregcamp/.icons/"

-- Font
--myFont = "-*-terminus-*-*-*-*-12-*-*-*-*-*-iso8859-*"
myFont = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"

-- Colors
myBgBgColor = "black"
myFgColor = "gray80"
myBgColor = "gray20"
myHighlightedFgColor = "white"
myHighlightedBgColor = "gray40"

myActiveBorderColor = "red50"
myInactiveBorderColor = "gray20"

myCurrentWsFgColor = "white"
myCurrentWsBgColor = "gray40"
myVisibleWsFgColor = "gray80"
myVisibleWsBgColor = "gray20"
myHiddenWsFgColor = "gray80"
myHiddenEmptyWsFgColor = "gray50"
myUrgentWsBgColor = "brown"
myTitleFgColor = "white"

myUrgencyHintFgColor = "white"
myUrgencyHintBgColor = "brown"

-- dzen general options
myDzenGenOpts = "-fg '" ++ myFgColor ++
                "' -bg '" ++ myBgColor ++
                "' -fn '" ++ myFont ++
                "' -h '16'"

-- Status Bar
myStatusBar = "dzen2 -w 1350 -ta l " ++ myDzenGenOpts

-- Conky Bar
myConkyBar = "conky -c ~/.conky_bar | dzen2 -x 1350 -w 1500 " ++ myDzenGenOpts

-- Layouts
myTheme :: Theme
myTheme = defaultTheme
    { activeColor = lightBackgroundColor
    , inactiveColor = backgroundColor
    , XMonad.Layout.Tabbed.urgentColor = backgroundColor
    , activeBorderColor = textColor
    , inactiveTextColor = textColor
    , urgentTextColor = textColor
    , inactiveBorderColor = lightBackgroundColor
    , urgentBorderColor = Main.urgentColor
    , activeTextColor = lightTextColor
    , fontName = myFont
    }

focusColor = "#ff3333"
textColor = "#c0c0a0"
lightTextColor = "#fffff0"
backgroundColor = "#304520"
lightBackgroundColor = "#456030"
urgentColor = "#ffc000"
 
myLayoutHook = smartBorders $ (tiled 
                            ||| Mirror tiled 
                            ||| Full
                            ||| tabbed shrinkText myTheme
                            ||| spiral (1 % 1)
                            ||| imLayout
                            )
    where
        tiled = ResizableTall nmaster delta ratio []
        nmaster = 1
        delta = 3/100
        ratio = 1/2


-- Workspaces
myWorkspaces =
    [
          wrapBitmap "sm4tik/arch_10x10.xbm"
        , wrapBitmap "sm4tik/fox.xbm"
        , wrapBitmap "sm4tik/dish.xbm"
        , wrapBitmap "sm4tik/cat.xbm"
        , wrapBitmap "sm4tik/empty.xbm"
        , wrapBitmap "sm4tik/shroom.xbm"
        , wrapBitmap "sm4tik/bug_02.xbm"
        , wrapBitmap "sm4tik/eye_l.xbm"
        , wrapBitmap "sm4tik/eye_r.xbm"
    ]
    where
        wrapBitmap bitmap = "^p(5)^i(" ++ myBitmapsPath ++ bitmap ++ ")^p(5)"

-- Urgency hint configurator
myUrgencyHook = withUrgencyHook dzenUrgencyHook
    {
        args = [
            "-x", "0", "-y", "576", "-h", "15", "-w", "1024",
            "-ta", "r",
            "-fg", "" ++ myUrgencyHintFgColor ++ "",
            "-bg", "" ++ myUrgencyHintBgColor ++ ""
            ]
    }

myManageHook = composeAll
    [ className =? "Gimp" --> doFloat ]

-- Prompt config
myXPConfig = defaultXPConfig 
    { position = Bottom
    , promptBorderWidth = 0
    , height = 15
    , bgColor = myBgColor
    , fgColor = myFgColor
    , fgHLight = myHighlightedFgColor
    , bgHLight = myHighlightedBgColor
    }

-- Union default and new key bindings
myKeys x = M.union (M.fromList (newKeys x)) (keys defaultConfig x)

-- Add new and/or redefine key bindings
newKeys conf@(XConfig {XMonad.modMask = modm}) = [
    -- Use shellPrompt instead of default dmenu
      ((modm, xK_p), shellPrompt myXPConfig)
    -- Do not leave unless conky, dzen and xxkb after restart
    , ((modm, xK_q), spawn "killall conky dzen2 xxkb; xmonad --recompile; xmonad --restart")
    , ((modm .|. controlMask, xK_x), spawn "gnome-screensaver-command --lock")
    , ((modm, xK_b), spawn "firefox")
    , ((modm, xK_u), spawn "dmenu_run -b -nb '#222222' -nf '#aaaaaa' -sb '#93d44f' -sf '#222222'")
    ]

-- Dzen config
myDzenPP h = defaultPP
    { ppOutput = hPutStrLn h
    , ppSep = "^bg(" ++ myBgBgColor ++ ")^r(1,15)^bg()"
    , ppWsSep = ""
    , ppCurrent = wrapFgBg myCurrentWsFgColor myCurrentWsBgColor
    , ppVisible = wrapFgBg myVisibleWsFgColor myVisibleWsBgColor
    , ppHidden = wrapFg myHiddenWsFgColor
    , ppHiddenNoWindows = wrapFg myHiddenEmptyWsFgColor
    , ppUrgent = wrapBg myUrgentWsBgColor
    , ppTitle = (\x -> " " ++ wrapFg myTitleFgColor x)
    , ppLayout = dzenColor myFgColor"" .
                    (\x -> case x of
                        "ResizableTall" -> wrapBitmap "dzen_bitmaps/tall.xbm"
                        "Mirror ResizableTall" -> wrapBitmap "dzen_bitmaps/mtall.xbm"
                        "Full" -> wrapBitmap "dzen_bitmaps/full.xbm"
                        "Tabbed Simplest" -> wrapBitmap "sm4tik/mail.xbm"
                        "Spiral" -> wrapBitmap "sm4tik/fs_01.xbm"
                        "IM" -> wrapBitmap "sm4tik/test.xbm"
                    )
    }
    where
        wrapFgBg fgColor bgColor content= wrap ("^fg(" ++ fgColor ++ ")^bg(" ++ bgColor ++ ")") "^fg()^bg()" content
        wrapFg color content = wrap ("^fg(" ++ color ++ ")") "^fg()" content
        wrapBg color content = wrap ("^bg(" ++ color ++ ")") "^bg()" content
        wrapBitmap bitmap = "^p(5)^i(" ++ myBitmapsPath ++ bitmap ++ ")^p(5)"
