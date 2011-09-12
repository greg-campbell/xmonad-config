import Data.List
import XMonad hiding (Tall)
import XMonad.Actions.GridSelect

import XMonad.Core

import XMonad.Actions.Promote
import XMonad.Actions.UpdatePointer
import XMonad.Actions.Warp
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.LayoutHints
import XMonad.Layout.PerWorkspace
import XMonad.Layout.HintedTile
import XMonad.Layout.Grid
import XMonad.Layout.ResizableTile
import XMonad.Layout.StackTile
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.IM
import XMonad.Hooks.FadeInactive

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Util.Run (spawnPipe)
 
import qualified Data.Map as M
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Util.Run (spawnPipe)
 
import qualified Data.Map as M
import Data.Ratio ((%))
import System.IO (hPutStrLn)
import GHC.IOBase (Handle)

 
 --{{{ Helper Functions
stripIM s = if ("IM " `isPrefixOf` s) then drop (length "IM ") s else s

wrapIcon icon = "^p(5)^i(" ++ icons ++ icon ++ ")^p(5)"
--}}}

--{{{ Path variables
icons = "/home/gregcamp/icons/"
--}}}

imLayout = smartBorders $ IM (1%5)
                          (Or (Title "Buddy List")
                          (And (Resource "main") (ClassName "psi")))
 
main :: IO ()

main = do
    --conkyBar <- spawnPipe myConkyBar
    --workspaceBarPipe <- spawnPipe myStatusBar
    --conkyBarPipe <- spawnPipe myConkyBar
    xmobar <- spawnPipe "xmobar"
    --myStatusBarPipe <- spawnPipe myStatusBar
    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
        { normalBorderColor  = backgroundColor
        , focusedBorderColor = focusColor
        --, terminal = "rxvt"
        , terminal = "gnome-terminal"
        , layoutHook = myLayout
        , manageHook = manageDocks
        , modMask = mod4Mask
        , borderWidth = 2
        , XMonad.Core.workspaces = myWorkspaces
        , keys = \c -> myKeys c `M.union` keys defaultConfig c
        --, logHook = dynamicLogWithPP (myPP myStatusBarPipe)
        --, logHook = dynamicLogWithPP (myPP conkyBar)
        , logHook = dynamicLogWithPP (myPP xmobar)
        --, logHook = myLogHook workspaceBarPipe >> fadeInactiveLogHook 0xdddddddd
                 >> updatePointer (Relative 1 1)
        }
        where
            myLayout = layoutHints $ avoidStruts $ smartBorders $ standardLayouts
                where standardLayouts = hintedTile Tall 
                                    ||| hintedTile Wide 
                                    ||| Full 
                                    ||| tabbed shrinkText myTheme 
                                    ||| spiral (1 % 1)
                                    ||| imLayout
                      hintedTile = HintedTile nmaster delta ratio TopLeft
                      --imLayout = withIM (2%10) (ClassName "Buddy List") Grid 
                      nmaster = 1
                      ratio   = 1/2
                      delta   = 3/100
 

            myPP :: Handle -> PP
            myPP din = defaultPP
                { ppCurrent = xmobarColor focusColor ""
                , ppVisible = xmobarColor lightTextColor ""
                , ppHiddenNoWindows = xmobarColor lightBackgroundColor ""
                , ppUrgent = xmobarColor urgentColor ""
                , ppSep = " · "
                , ppWsSep = ""
                , ppTitle = xmobarColor lightTextColor ""
                , ppOutput = hPutStrLn din
                }
 
            myTheme :: Theme
            myTheme = defaultTheme
                { activeColor = lightBackgroundColor
                , inactiveColor = backgroundColor
                , urgentColor = backgroundColor
                , activeBorderColor = textColor
                , inactiveTextColor = textColor
                , urgentTextColor = textColor
                , inactiveBorderColor = lightBackgroundColor
                , urgentBorderColor = urgentColor
                , activeTextColor = lightTextColor
                , fontName = myFont
                }
 
            myXPConfig :: XPConfig
            myXPConfig = defaultXPConfig
                { font        = myFont
                , bgColor     = backgroundColor
                , fgColor     = textColor
                , fgHLight    = lightTextColor
                , bgHLight    = lightBackgroundColor
                , borderColor = lightBackgroundColor
                }

            --Font
            myFont = "Terminus-6"

            -- Colors

            --- Main Colours
            myFgColor = "#aaaaaa"
            myBgColor = "#222222"
            myHighlightedFgColor = myFgColor
            myHighlightedBgColor = "#93d44f"

            --- Borders
            myActiveBorderColor = myCurrentWsBgColor
            myInactiveBorderColor = "#555753"
            myBorderWidth = 2

            --- Ws Stuff
            myCurrentWsFgColor = "#222222"
            myCurrentWsBgColor = myHighlightedBgColor
            myVisibleWsFgColor = myBgColor
            myVisibleWsBgColor = "#c8e7a8"
            myHiddenWsFgColor = "#FFFFFF"
            myHiddenEmptyWsFgColor = "#8F8F8F"
            myUrgentWsBgColor = "#ff6565"
            myTitleFgColor = myFgColor


            --- Urgency
            myUrgencyHintFgColor = "#000000"
            myUrgencyHintBgColor = "#ff6565"

            -- }}}
 
            myStatusBar = "dzen2 -w 1920 -ta l " ++ myDzenGenOpts
            myConkyBar = "sleep 1 && conky -c ~/.conky_bar | dzen2 -x 0 -y 1000 -w 1920  -ta l " ++ myDzenGenOpts
            myDzenGenOpts  =  "-fg '" ++ myFgColor ++ "' -bg '" ++ myBgColor ++ "' -h '18'" ++ "-sa c -e 'onstart=lower' -fn '" ++ myFont ++ "'"

            --myFont = "xft:DejaVu Sans:size=10"
            focusColor = "#ff3333"
            textColor = "#c0c0a0"
            lightTextColor = "#fffff0"
            backgroundColor = "#304520"
            lightBackgroundColor = "#456030"
            urgentColor = "#ffc000"

            myKeys conf@(XConfig {XMonad.modMask = modMask, workspaces = ws}) = M.fromList $
                [ ((modMask,                 xK_Return), promote)
                , ((modMask .|. controlMask, xK_b), sendMessage ToggleStruts)
                , ((modMask .|. controlMask, xK_x), shellPrompt myXPConfig)
                , ((modMask .|. controlMask, xK_s), sshPrompt myXPConfig)
                , ((modMask,                 xK_z), warpToWindow 1 1)
                , ((modMask,                 xK_l), spawn "gnome-screensaver-command --lock")
                , ((modMask,                 xK_b), spawn "firefox")
                , ((modMask,                 xK_t), spawn "gnome-terminal")
                , ((modMask,                 xK_u), spawn "dmenu_run -b -nb '#222222' -nf '#aaaaaa' -sb '#93d44f' -sf '#222222'")
                , ((modMask,                 xK_q), recompile True >> restart "xmonad" True)
                , ((modMask .|. controlMask, xK_q), spawn "killall conky dzen2 && xmonad --recompile && xmonad --restart")
                , ((modMask .|. controlMask, xK_l), spawn "exec xlogo -render -fg `randomdarkcolor` -bg `randomdarkcolor`") -- For testing layouts.
                ]
             
             -- Workspaces
            myWorkspaces =
                [
                " 1 sh ",
                " 2 vi ",
                " 3 www ",
                " 4 ",
                " 5 ",
                " 6 ",
                " 7 ",
                " . "
                ]
         
         ---{{{ Dzen Config
            myDzenPP h = defaultPP {
                 ppOutput = hPutStrLn h,
                 ppSep = (wrapFg myHighlightedBgColor "|"),
                 ppWsSep = "",
                 ppCurrent = wrapFgBg myCurrentWsFgColor myCurrentWsBgColor,
                 ppVisible = wrapFgBg myVisibleWsFgColor myVisibleWsBgColor,
                 ppHidden = wrapFg myHiddenWsFgColor . noScratchPad,
                 ppHiddenNoWindows = wrapFg myHiddenEmptyWsFgColor,
                 ppUrgent = wrapBg myUrgentWsBgColor,
                 ppTitle = (\x -> "  " ++ wrapFg myTitleFgColor x),
                 ppLayout  = dzenColor myFgColor"" .
                               (\x -> case x of
                                   "ResizableTall" -> wrapIcon "dzen_bitmaps/tall.xbm"
                                   "Mirror ResizableTall" -> wrapIcon "dzen_bitmaps/mtall.xbm"
                                   "Full" -> wrapIcon "dzen_bitmaps/full.xbm"
                               ) . stripIM
             }
             where
                 wrapFgBg fgColor bgColor content= wrap ("^fg(" ++ fgColor ++ ")^bg(" ++ bgColor ++ ")") "^fg()^bg()" content
                 wrapFg color content = wrap ("^fg(" ++ color ++ ")") "^fg()" content
                 wrapBg color content = wrap ("^bg(" ++ color ++ ")") "^bg()" content
                 noScratchPad ws = if ws == "NSP" then "" else ws
         --}}}

