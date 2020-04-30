{-# LANGUAGE OverloadedStrings #-}
import XMonad
import XMonad.Config.Kde
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise (runOrRaisePrompt)
import XMonad.Prompt.AppendFile (appendFilePrompt)
import XMonad.Operations
import System.IO
import System.Exit
import XMonad.Util.Run
import XMonad.Util.NamedWindows (getName)
import XMonad.Actions.CycleWS
import XMonad.Actions.PhysicalScreens
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
import XMonad.Layout.NoBorders (smartBorders, noBorders)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.IM
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spiral
import XMonad.Layout.NoBorders
import XMonad.Layout.Gaps
import Data.List (sortBy, group)
import Data.Function (on)
import Control.Monad (forM_, join)

import qualified XMonad.StackSet as W
import qualified Data.Map as M
import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

import Graphics.X11.ExtraTypes.XF86

myTerminal      = "tilix"
-- Define modMask
modMask' :: KeyMask
modMask' = mod4Mask
-- Define workspaces
myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]

--}}}
-- Main {{{
main :: IO()
main = do
  --  spawn "sh /home/dave/.xmonad/autostart.sh"
    forM_ [".xmonad-workspace-log", ".xmonad-title-log"] $ \file -> do
      safeSpawn "mkfifo" ["/tmp/" ++ file]

    dbus <- D.connectSession
    getWellKnownName dbus

    xmonad $ docks $ kde4Config
      { terminal            = myTerminal
      , workspaces          = myWorkspaces
      , keys                = keys'
      , modMask             = modMask'
      , startupHook         = ewmhDesktopsStartup >> setWMName "xmonad"
      , layoutHook          = layoutHook'
      , manageHook          = manageHook'
      , logHook             = eventLogHook
      , normalBorderColor   = colorNormalBorder
      , focusedBorderColor  = colorFocusedBorder
      , XMonad.focusFollowsMouse = False
      , XMonad.clickJustFocuses = False
}
--}}}
-- Hooks {{{
-- ManageHook {{{
manageHook' :: ManageHook
manageHook' = (composeAll . concat $
    [[manageHook kde4Config]
    , [resource     =? r            --> doIgnore            |   r   <- myIgnores] -- ignore desktop
    , [className    =? c            --> doCenterFloat       |   c   <- myFloats ] -- float my floats
    , [name         =? n            --> doCenterFloat       |   n   <- myNames  ] -- float my names
    , [isFullscreen                 --> myDoFullFloat                           ]
    ]) 

    where
        role      = stringProperty "WM_WINDOW_ROLE"
        name      = stringProperty "WM_NAME"

        -- WM_CLASS to float
        myFloats  = [
          "MPlayer","Zenity","Xmessage",
          "Save As...","Save Image","XFontSel",
          "Downloads","downloads", 
          "plasmashell"]

        -- resources
        myIgnores =  ["xfdesktop", "desktop","desktop_window","notify-osd","stalonetray","trayer"]

        -- WM_NAME to float
        myNames   = ["bashrun","Google Chrome Options","Chromium Options", "Microsoft Teams Notification"]

-- a trick for fullscreen but stil allow focusing of other WSs
myDoFullFloat :: ManageHook
myDoFullFloat = doF W.focusDown <+> doFullFloat
-- }}}
layoutHook' = customLayout

-- Layout
-- -- test out golden ratio -- $ spiral (1.6/1)  
customLayout = gaps [] $ avoidStruts $ spacing 2 $ smartBorders tiled ||| smartBorders (Mirror tiled)  ||| noBorders Full ||| smartBorders simpleFloat
  where
    --tiled = ResizableTall 1 (2/100) (1/2) []
    tiled   = ResizableTall nmaster delta ratio []
    nmaster = 1   
    delta   = 2/100
    ratio   = 1/2
--}}}
-- Theme {{{
-- Color names are easier to remember:
colorOrange          = "#ff7701"
colorDarkGray        = "#171717"
colorPink            = "#e3008d"
colorGreen           = "#00aa4a"
colorBlue            = "#008dd5"
colorYellow          = "#fee100"
colorWhite           = "#cfbfad"
 
colorNormalBorder    = "#1c2636"
colorFocusedBorder   = "#EA5460"
barFont  = "inconsolata:size=14"
barXFont = "inconsolata:size=14"
xftFont = "xft: inconsolata-14"
--}}}

-- Prompt Config {{{
mXPConfig :: XPConfig
mXPConfig =
    defaultXPConfig { font                  = barFont
                    , bgColor               = colorDarkGray
                    , fgColor               = colorGreen
                    , bgHLight              = colorGreen
                    , fgHLight              = colorDarkGray
                    , promptBorderWidth     = 0
                    , height                = 12
                    , historyFilter         = deleteConsecutive
                    }
 
-- Run or Raise Menu
largeXPConfig :: XPConfig
largeXPConfig = mXPConfig
                { font = xftFont
                , height = 20
                }
-- }}}
-- Key mapping {{{
keys' :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys' conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask,                    xK_p        ), runOrRaisePrompt largeXPConfig)
    , ((mod1Mask,                   xK_F2       ), spawn "gmrun")
    , ((0,                          xK_Print    ), spawn "screenshot scr")

    -- Programs
    , ((modMask,                    xK_t        ), spawn $ XMonad.terminal conf) -- spawn terminal
    , ((modMask,                    xK_f        ), spawn "firefox")
    , ((modMask,                    xK_v        ), spawn "code")
    , ((modMask,                    xK_x        ), spawn "rofi -show run")
    , ((modMask,                    xK_c        ), spawn "chromium-browser")
    , ((modMask,                    xK_g        ), spawn "gimp")
    , ((modMask,                    xK_s        ), spawn "spotify")

    -- layouts
    , ((modMask,                    xK_space    ), sendMessage NextLayout)
    , ((modMask .|. shiftMask,      xK_space    ), setLayout $ XMonad.layoutHook conf) -- reset layout on current desktop to default
    , ((modMask,                    xK_b        ), sendMessage ToggleStruts)
    , ((mod1Mask,                   xK_Tab      ), windows W.focusDown) -- move focus to next window
    , ((mod1Mask,                   xK_F4       ), kill) -- kill selected window
    , ((modMask .|. shiftMask,      xK_j        ), windows W.swapDown) -- swap the focused window with the next window
    , ((modMask .|. shiftMask,      xK_k        ), windows W.swapUp)  -- swap the focused window with the previous window
    , ((modMask .|. shiftMask,      xK_t        ), withFocused $ windows . W.sink) -- Push window back into tiling

    -- workspaces
    , ((mod1Mask .|. controlMask,   xK_Right    ), nextWS)
    , ((mod1Mask .|. shiftMask,     xK_Right    ), shiftToNext)
    , ((mod1Mask .|. controlMask,   xK_Left     ), prevWS)
    , ((mod1Mask .|. shiftMask,     xK_Left     ), shiftToPrev)

    -- resizing the window ratio
    , ((modMask,               xK_h     ), sendMessage Shrink) -- %! Shrink the master area
    , ((modMask,               xK_l     ), sendMessage Expand) -- %! Expand the master area    

    -- quit, or restart
    , ((modMask .|. shiftMask,      xK_q        ), restart "xmonad" True)
    ]

    ++

    -- Volume control
    [ ((0, xF86XK_AudioLowerVolume   ), spawn "amixer -q -D pulse sset Master 2%-")
    , ((0, xF86XK_AudioRaiseVolume   ), spawn "amixer -q -D pulse sset Master 2%+")
    , ((0, xF86XK_AudioMute          ), spawn "amixer -q -D pulse sset Master toggle")
    ]
    ++

    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    ++
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
--}}}

getWellKnownName :: D.Client -> IO ()
getWellKnownName dbus = do
  D.requestName dbus (D.busName_ "org.xmonad.Log")
                [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  return ()
  
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal "/org/xmonad/Log" "org.xmonad.Log" "Update") {
            D.signalBody = [D.toVariant ("<b>" ++ (UTF8.decodeString str) ++ "</b>")]
        }
    D.emit dbus signal


eventLogHook = do
  winset <- gets windowset
  title <- maybe (return "") (fmap show . getName) . W.peek $ winset
  let currWs = W.currentTag winset
  let wss = map W.tag $ W.workspaces winset
  let wsStr = join $ map (fmt currWs) $ sort' wss

  io $ appendFile "/tmp/.xmonad-title-log" (title ++ "\n")
  io $ appendFile "/tmp/.xmonad-workspace-log" (wsStr ++ "\n")

  where fmt currWs ws
          | currWs == ws = "[" ++ ws ++ "]"
          | otherwise    = " " ++ ws ++ " "
        sort' = sortBy (compare `on` (!! 0))
