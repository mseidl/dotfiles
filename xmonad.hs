------------------------------------------------------------------------
-- ~/.xmonad/xmonad.hs
-- validate syntax: xmonad --recompile
{-# LANGUAGE NoMonomorphismRestriction #-}
------------------------------------------------------------------------
import XMonad hiding (Tall)
import XMonad.Actions.CycleWS
import XMonad.Actions.FloatKeys
import XMonad.Actions.GridSelect
import XMonad.Actions.SpawnOn
import XMonad.Hooks.DynamicHooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.ComboP
import XMonad.Layout.LayoutCombinators hiding ((|||))
import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane
import XMonad.ManageHook
import XMonad.Prompt
import XMonad.Prompt.Shell
--import XMonad.Util.Cursor
import XMonad.Util.Run
import Control.Monad (liftM2)
import Data.Monoid
import Graphics.X11
import Graphics.X11.Xinerama
import System.Exit
import System.IO

import qualified XMonad.Actions.FlexibleResize as Flex
import qualified XMonad.StackSet as W
import qualified Data.Map as M

main = do
  dzen <- spawnPipe myStatusBar
  conkytop <- spawnPipe myTopBar
  conkympd <- spawnPipe myMPDBar
  conkyhdd <- spawnPipe myHDDBar
  xmonad $ myUrgencyHook $ defaultConfig
        { terminal           = myTerminal
        , modMask            = myModMask
        , focusFollowsMouse  = myFocusFollowsMouse
        , borderWidth        = myBorderWidth
        , workspaces         = myWorkSpaces
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , keys               = myKeys
     -- , mouseBindings      = myMouseBindings

        , layoutHook         = myLayout
        , manageHook         = myManageHook <+> manageDocks <+> dynamicMasterHook <+> manageSpawn
        , handleEventHook    = myEventHook
        , logHook            = dynamicLogWithPP $ myDzenPP_ dzen
        , startupHook        = myStartupHook
        }

myTerminal = "urxvt-256color"
myModMask = mod1Mask
myFocusFollowsMouse = True
myBorderWidth = 1
myWorkSpaces = ["1:work", "2:web", "3:com", "4:music"]
myNormalBorderColor = "#0f0f0f"
myFocusedBorderColor = "#1f1f1f"
myEventHook = mempty
--myLogHook = dynamicLogWithPP $ myDzenPP dzen
myStartupHook = return ()
--myStartupHook = setDefaultCursor xC_left_ptr


-- Color, font and iconpath definitions:
myFont = "-xos4-terminus-medium-r-normal-*-12-*-*-*-c-*-iso10646-1"
--myFont = "-*-montecarlo-medium-r-normal-*-11-*-*-*-c-*-*-*"
--myFont = "-*-proggyopti-medium-r-normal-*-11-*-*-*-c-*-*-*"
--myFont = "-*-proggytinysz-medium-r-normal-*-10-*-*-*-c-*-*-*"
myIconDir = "/foo/bar/.dzen"
myDzenFGColor = "#555555"
myDzenBGColor = "#222222"
myNormalFGColor = "#ffffff"
myNormalBGColor = "#0f0f0f"
myFocusedFGColor = "#f0f0f0"
myFocusedBGColor = "#333333"
myUrgentFGColor = "#0099ff"
myUrgentBGColor = "#0077ff"
myIconFGColor = "#777777"
myIconBGColor = "#0f0f0f"
myPatternColor = "#1f1f1f"
mySeperatorColor = "#555555"
-- GSConfig options:
myGSConfig = defaultGSConfig
    { gs_cellheight = 50
    , gs_cellwidth = 250
    , gs_cellpadding = 10
    --, gs_colorizer = ""
    , gs_font = "" ++ myFont ++ ""
    --, gs_navigate = ""
    --, gs_originFractX = ""
    --, gs_originFractY = ""
    }

-- XPConfig options:
myXPConfig = defaultXPConfig
    { font = "" ++ myFont ++ ""
    , bgColor = "" ++ myNormalBGColor ++ ""
    , fgColor = "" ++ myNormalFGColor ++ ""
    , fgHLight = "" ++ myNormalFGColor ++ ""
    , bgHLight = "" ++ myUrgentBGColor ++ ""
    , borderColor = "" ++ myFocusedBorderColor ++ ""
    , promptBorderWidth = 1
    , position = Bottom
    , height = 16
    , historySize = 100
    --, historyFilter = ""
    --, promptKeymap = ""
    --, completionKey = ""
    --, defaultText = ""
    --, autoComplete = "KeySym"
    --, showCompletionOnTab = ""
    }
-- Theme options:
myTheme = defaultTheme
    { activeColor = "" ++ myFocusedBGColor ++ ""
    , inactiveColor = "" ++ myDzenBGColor ++ ""
    , urgentColor = "" ++ myUrgentBGColor ++ ""
    , activeBorderColor = "" ++ myFocusedBorderColor ++ ""
    , inactiveBorderColor = "" ++ myNormalBorderColor ++ ""
    , urgentBorderColor = "" ++ myNormalBorderColor ++ ""
    , activeTextColor = "" ++ myFocusedFGColor ++ ""
    , inactiveTextColor = "" ++ myDzenFGColor ++ ""
    , urgentTextColor = "" ++ myUrgentFGColor ++ ""
    , fontName = "" ++ myFont ++ ""
    --, decoWidth = ""
    --, decoHeight = ""
    }

-- Window rules:
myManageHook = composeAll . concat $
    [ [isDialog --> doFloat]
    , [className =? c --> doFloat | c <- myCFloats]
    , [title =? t --> doFloat | t <- myTFloats]
    , [resource =? r --> doFloat | r <- myRFloats]
    , [resource =? i --> doIgnore | i <- myIgnores]
    , [(className =? "XTerm" --> doFloat)]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift "1:irc" | x <- my1Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift "2:www" | x <- my2Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift "3:music" | x <- my3Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "4:misc" | x <- my4Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "5:xbmc" | x <- my5Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "6:GIMP" | x <- my6Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "7:slideshow!" | x <- my7Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "8:foo()" | x <- my8Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "9:vbox" | x <- my9Shifts]
    ]
    where
    doShiftAndGo = doF . liftM2 (.) W.greedyView W.shift
    myCFloats = ["Ekiga", "MPlayer", "Nitrogen", "Nvidia-settings", "Sysinfo", "XCalc", "XFontSel", "Xmessage"]
    myTFloats = ["Downloads", "Iceweasel Preferences", "Save As..."]
    myRFloats = []
    myIgnores = ["desktop_window", "kdesktop"]
    my1Shifts = []
    my2Shifts = ["Chromium"]
    my3Shifts = ["Deadbeef", "Gmpc"]
    my4Shifts = ["Eog", "Evince", "Gthumb", "Nautilus", "Pcmanfm", "Pinta"]
    my5Shifts = ["MPlayer", "xbmc.bin"]
    my6Shifts = ["Gimp"]
    my7Shifts = ["feh"]
    my8Shifts = ["Easytag", "Gconf-editor", "Inkscape", "K3b", "MusicBrainz Picard", "tmw", "Twf", "VCLSalFrame.DocumentWindow"]
    my9Shifts = ["VirtualBox", "Wine"]
-- Statusbar options:
myStatusBar = "dzen2 -x '0' -y '0' -h '16' -w '1300' -ta 'l' -fg '" ++ myNormalFGColor ++ "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont ++ "'"
myTopBar = "conky -c .conkytop | dzen2 -x '1300' -y '0' -h '16' -w '620' -ta 'r' -fg '" ++ myDzenFGColor ++ "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont ++ "'"
myMPDBar = "conky -c .conkympd | dzen2 -x '0' -y '1184' -h '16' -w '1600' -ta 'l' -fg '" ++ myDzenFGColor ++ "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont ++ "'"
myHDDBar = "conky -c .conkyhdd | dzen2 -x '1600' -y '1184' -h '16' -w '320' -ta 'r' -fg '" ++ myDzenFGColor ++ "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont ++ "'"

-- Urgency hint options:
myUrgencyHook = withUrgencyHook dzenUrgencyHook
    { args = ["-x", "0", "-y", "1200", "-h", "16", "-w", "1920", "-ta", "r", "-expand", "l", "-fg", "" ++ myUrgentFGColor ++ "", "-bg", "" ++ myNormalBGColor ++ "", "-fn", "" ++ myFont ++ ""] }

-- Layouts:
myLayout = avoidStruts $ layoutHints $ onWorkspace "1:work" (resizableTile ||| Mirror resizableTile) $ smartBorders (Full ||| resizableTile ||| Mirror resizableTile)
    where
    resizableTile = ResizableTall nmaster delta ratio []
    tabbedLayout = tabbedBottomAlways shrinkText myTheme
    --gimpLayout = tabbedLayout ****||* Full
    --gimpLayout = Full *||**** tabbedLayout
    --gimpLayout = combineTwoP (TwoPane 0.04 0.82) (tabbedLayout) (Full) (Not (Role "gimp-toolbox"))
    nmaster = 1
    ratio = toRational (2/(1+sqrt(5)::Double))
    delta = 3/100

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((mod4Mask, xK_e), spawn "google-chrome")
    , ((mod4Mask, xK_l), spawn "xlock")
    , ((mod4Mask, xK_r), spawn "urxvt-256color")
    , ((mod4Mask .|. shiftMask, xK_s), spawn "mpc | osd_cat -p bottom -A center -s 2 -c '#a10a30' -f '-xos4-terminus-medium-r-normal-*-50-*-*-*-c-*-iso10646-1'")
    , ((mod4Mask, xK_d), spawn "date +'%Y-%m-%d %H:%M' | osd_cat -p bottom -A center -s 2 -c '#a10a30' -f '-xos4-terminus-medium-r-normal-*-140-*-*-*-c-*-iso10646-1'")
    , ((modMask, xK_space), spawn "dmenu_run")
    , ((modMask .|. controlMask, xK_r), spawn "killall conky dzen2 && xmonad --recompile && xmonad --restart") -- restart xmonad
    , ((modMask .|. controlMask, xK_Home), spawn "mpc toggle") -- play/pause song
    , ((modMask .|. controlMask, xK_End), spawn "mpc stop") -- stop playback
    , ((modMask .|. controlMask, xK_Prior), spawn "mpc prev") -- previous song
    , ((modMask .|. controlMask, xK_Next), spawn "mpc next") -- next song
    , ((modMask .|. shiftMask, xK_Tab), windows W.focusUp) -- move focus to the previous window
    , ((modMask .|. shiftMask, xK_g), gridselectWorkspace myGSConfig W.view) -- display grid select and go to selected workspace
    , ((modMask .|. shiftMask, xK_h), sendMessage Shrink) -- shrink the master area
    , ((modMask .|. shiftMask, xK_l), sendMessage Expand) -- expand the master area

    , ((modMask .|. shiftMask, xK_j), windows W.swapDown) -- swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_k), windows W.swapUp)  -- swap the focused window with the previous window
    , ((modMask .|. controlMask, xK_h), sendMessage MirrorExpand) -- expand the height/width
    , ((modMask .|. controlMask, xK_j), windows W.swapDown) -- swap the focused window with the next window
    , ((modMask .|. controlMask, xK_k), windows W.swapUp)  -- swap the focused window with the previous window
    , ((modMask .|. controlMask, xK_l), sendMessage MirrorShrink) -- shrink the height/width
    , ((modMask, xK_f), sendMessage NextLayout) -- rotate through the available layout algorithms
    , ((modMask, xK_g), goToSelected myGSConfig) -- display grid select and go to selected window
--    , ((modMask, xK_Left), prevWS) -- switch to previous workspace
--    , ((modMask, xK_Right), nextWS) -- switch to next workspace
    , ((modMask .|. controlMask, xK_x), kill) -- close focused window
    , ((modMask .|. controlMask, xK_d), withFocused $ windows . W.sink) -- push window back into tiling
    , ((modMask .|. controlMask, xK_f), setLayout $ XMonad.layoutHook conf) -- reset the layouts on the current workspace to default

    ]  
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


-- dynamicLog pretty printer for dzen:
myDzenPP h = defaultPP
    { ppCurrent = wrap ("^fg(" ++ myUrgentFGColor ++ ")^bg(" ++ myFocusedBGColor ++ ")^p()^i(" ++ myIconDir ++ "/corner.xbm)^fg(" ++ myNormalFGColor ++ ")") "^fg()^bg()^p()" . \wsId -> dropIx wsId
    , ppVisible = wrap ("^fg(" ++ myNormalFGColor ++ ")^bg(" ++ myFocusedBGColor ++ ")^p()^i(" ++ myIconDir ++ "/corner.xbm)^fg(" ++ myNormalFGColor ++ ")") "^fg()^bg()^p()" . \wsId -> dropIx wsId
    , ppHidden = wrap ("^i(" ++ myIconDir ++ "/corner.xbm)") "^fg()^bg()^p()" . \wsId -> if (':' `elem` wsId) then drop 2 wsId else wsId -- don't use ^fg() here!!
    --, ppHiddenNoWindows = wrap ("^fg(" ++ myDzenFGColor ++ ")^bg()^p()^i(" ++ myIconDir ++ "/corner.xbm)") "^fg()^bg()^p()" . \wsId -> dropIx wsId
    , ppHiddenNoWindows = \wsId -> if wsId `notElem` staticWs then "" else wrap ("^fg(" ++ myDzenFGColor ++ ")^bg()^p()^i(" ++ myIconDir ++ "/corner.xbm)") "^fg()^bg()^p()" . dropIx $ wsId
    , ppUrgent = wrap (("^fg(" ++ myUrgentFGColor ++ ")^bg(" ++ myNormalBGColor ++ ")^p()^i(" ++ myIconDir ++ "/corner.xbm)^fg(" ++ myUrgentFGColor ++ ")")) "^fg()^bg()^p()" . \wsId -> dropIx wsId
    , ppSep = " "
    , ppWsSep = " "
    , ppTitle = dzenColor ("" ++ myNormalFGColor ++ "") "" . wrap "< " " >"
    , ppLayout = dzenColor ("" ++ myNormalFGColor ++ "") "" .
        (\x -> case x of
        "Hinted Full" -> "^fg(" ++ myIconFGColor ++ ")^i(" ++ myIconDir ++ "/layout-full.xbm)"
        "Hinted ResizableTall" -> "^fg(" ++ myIconFGColor ++ ")^i(" ++ myIconDir ++ "/layout-tall-right.xbm)"
        "Hinted Mirror ResizableTall" -> "^fg(" ++ myIconFGColor ++ ")^i(" ++ myIconDir ++ "/layout-mirror-bottom.xbm)"
        --"Hinted combining Tabbed Bottom Simplest and Full with DragPane  Vertical 0.1 0.8" -> "^fg(" ++ myIconFGColor ++ ")^i(" ++ myIconDir ++ "/layout-gimp.xbm)"
        "Hinted combining Tabbed Bottom Simplest and Full with TwoPane using Not (Role \"gimp-toolbox\")" -> "^fg(" ++ myIconFGColor ++ ")^i(" ++ myIconDir ++ "/layout-gimp.xbm)"
        _ -> x
        )
    , ppOutput = hPutStrLn h
    }
    where
    dropIx wsId = if (':' `elem` wsId) then drop 2 wsId else wsId
    staticWs = ["1:work", "2:www", "3:dev", "4:music", "5:com"]

-- dynamicLog pretty printer for dzen:
myDzenPP_ h = defaultPP
    { ppCurrent = wrap ("^p(2)^ib(1)^fg(" ++ myFocusedBGColor ++ ")^i(" ++ myIconDir ++ "/corner_left.xbm)^r(1300x12)^p(-1300)^fg(" ++ myUrgentFGColor ++ ")^bg(" ++ myFocusedBGColor ++ ")^p()``^fg(" ++ myNormalFGColor ++ ")^p(2)") ("^p(2)^fg(" ++ myFocusedBGColor ++ ")^i(" ++ myIconDir ++ "/corner_right.xbm)^fg(" ++ myNormalBGColor ++ ")^r(1300x12)^p(-1300)^ib(0)^fg()^bg()^p()") . \wsId -> dropIx wsId
    , ppVisible = wrap ("^p(2)^ib(1)^fg(" ++ myPatternColor ++ ")^i(" ++ myIconDir ++ "/corner_left.xbm)^r(1300x12)^p(-1300)^fg(" ++ myNormalFGColor ++ ")^bg(" ++ myFocusedBGColor ++ ")^p()``^fg(" ++ myNormalFGColor ++ ")^p(2)") ("^p(2)^fg(" ++ myPatternColor ++ ")^i(" ++ myIconDir ++ "/corner_right.xbm)^fg(" ++ myNormalBGColor ++ ")^r(1300x12)^p(-1300)^ib(0)^fg()^bg()^p()") . \wsId -> dropIx wsId
    , ppHidden = wrap ("^p(2)^ib(1)^fg(" ++ myPatternColor ++ ")^i(" ++ myIconDir ++ "/corner_left.xbm)^r(1300x12)^p(-1300)^fg()^bg()^p()``^p(2)") ("^p(2)^fg(" ++ myPatternColor ++ ")^i(" ++ myIconDir ++ "/corner_right.xbm)^fg(" ++ myNormalBGColor ++ ")^r(1300x12)^p(-1300)^p()^ib(0)^fg()^bg()^p()") . \wsId -> if (':' `elem` wsId) then drop 2 wsId else wsId -- don't use ^fg() here!!
    , ppHiddenNoWindows = \wsId -> if wsId `notElem` staticWs then "" else wrap ("^p(2)^ib(1)^fg(" ++ myPatternColor ++ ")^i(" ++ myIconDir ++ "/corner_left.xbm)^r(1300x12)^p(-1300)^fg(" ++ myDzenFGColor ++ ")^bg()^p()``^p(2)") ("^p(2)^fg(" ++ myPatternColor ++ ")^i(" ++ myIconDir ++ "/corner_right.xbm)^fg(" ++ myNormalBGColor ++ ")^r(1300x12)^p(-1300)^ib(0)^fg()^bg()^p()") . dropIx $ wsId
    , ppUrgent = wrap (("^p(2)^ib(1)^fg(" ++ myPatternColor ++ ")^i(" ++ myIconDir ++ "/corner_left.xbm)^r(1300x12)^p(-1300)^fg(" ++ myUrgentFGColor ++ ")^bg(" ++ myNormalBGColor ++ ")^p()``^fg(" ++ myUrgentFGColor ++ ")^p(2)")) ("^p(2)^fg(" ++ myPatternColor ++ ")^i(" ++ myIconDir ++ "/corner_right.xbm)^fg(" ++ myNormalBGColor ++ ")^r(1300x12)^p(-1300)^ib(0)^fg()^bg()^p()") . \wsId -> dropIx wsId
    , ppSep = " "
    , ppWsSep = ""
    , ppTitle = dzenColor ("" ++ myNormalFGColor ++ "") "" . wrap ("^ib(1)^fg(" ++ myPatternColor ++ ")^i(" ++ myIconDir ++ "/corner_left.xbm)^r(1300x12)^p(-1300)^p(2)^fg()< ") (" >^p(2)^fg(" ++ myPatternColor ++ ")^i(" ++ myIconDir ++ "/corner_right.xbm)^fg(" ++ myNormalBGColor ++ ")^r(1300x12)^p(-1300)^ib(0)^fg()")
    , ppLayout = dzenColor ("" ++ myNormalFGColor ++ "") "" .
        (\x -> case x of
        "Hinted Full" -> "^fg(" ++ myIconFGColor ++ ")^i(" ++ myIconDir ++ "/layout-full.xbm)"
        "Hinted ResizableTall" -> "^fg(" ++ myIconFGColor ++ ")^i(" ++ myIconDir ++ "/layout-tall-right.xbm)"
        "Hinted Mirror ResizableTall" -> "^fg(" ++ myIconFGColor ++ ")^i(" ++ myIconDir ++ "/layout-mirror-bottom.xbm)"
        --"Hinted combining Tabbed Bottom Simplest and Full with DragPane  Vertical 0.1 0.8" -> "^fg(" ++ myIconFGColor ++ ")^i(" ++ myIconDir ++ "/layout-gimp.xbm)"
        "Hinted combining Tabbed Bottom Simplest and Full with TwoPane using Not (Role \"gimp-toolbox\")" -> "^fg(" ++ myIconFGColor ++ ")^i(" ++ myIconDir ++ "/layout-gimp.xbm)"
        _ -> x
        )
    , ppOutput = hPutStrLn h
    }
    where
    dropIx wsId = if (':' `elem` wsId) then drop 2 wsId else wsId
    staticWs = ["1:work", "2:www", "3:dev", "4:music", "5:com"]


