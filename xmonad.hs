import           Control.Monad                       (unless, when)
import           Data.Bits                           (testBit)
import           Data.Foldable                       (find)
import qualified Data.Map                            as M
import           Data.Maybe                          (fromJust)
import           Data.Semigroup
import           Foreign.C                           (CInt)
import           Graphics.X11.ExtraTypes.XF86
import           Graphics.X11.Xinerama               (getScreenInfo)
import           Prelude                             hiding (log)
import           System.Exit
import           System.IO
import           XMonad
import qualified XMonad.StackSet                     as W

import           XMonad.Hooks.DynamicLog
--import           XMonad.Hooks.DynamicProperty
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.InsertPosition         (Focus (Newer),
                                                      Position (End, Master),
                                                      insertPosition)
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.RefocusLast            (isFloat)
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.StatusBar
import           XMonad.Hooks.UrgencyHook
import           XMonad.Hooks.WindowSwallowing

import           XMonad.Layout.Decoration            (ModifiedLayout)
import           XMonad.Layout.DraggingVisualizer    (draggingVisualizer)
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.HintedGrid
import           XMonad.Layout.IndependentScreens
import           XMonad.Layout.MouseResizableTile
import           XMonad.Layout.MultiToggle           (EOT (EOT),
                                                      Toggle (Toggle), mkToggle,
                                                      (??))
import           XMonad.Layout.MultiToggle.Instances (StdTransformers (NBFULL))
--import           XMonad.Layout.Named                 (named)
import           XMonad.Layout.NoBorders
import           XMonad.Layout.NoBorders             (smartBorders)
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Spacing               (Border (Border), Spacing,
                                                      spacingRaw)
import           XMonad.Layout.Spiral
import           XMonad.Layout.Tabbed
import           XMonad.Layout.ThreeColumns

import qualified XMonad.Util.ExtensibleState         as XS
import           XMonad.Util.EZConfig                (additionalKeysP)
import           XMonad.Util.Loggers                 (logLayoutOnScreen,
                                                      logTitleOnScreen,
                                                      shortenL, wrapL,
                                                      xmobarColorL)
import           XMonad.Util.Run                     (runInTerm, spawnPipe)
import           XMonad.Util.SpawnOnce

import           Data.List
import qualified Data.List                           as L
import           XMonad.Actions.CycleWS
import qualified XMonad.Actions.FlexibleResize       as Flex
import           XMonad.Actions.OnScreen             (onlyOnScreen)
import           XMonad.Actions.TiledWindowDragging
import           XMonad.Actions.UpdateFocus
import           XMonad.Actions.UpdatePointer        (updatePointer)
import           XMonad.Actions.Warp                 (warpToScreen)
import           XMonad.Actions.WindowGo             (runOrRaise)


--------------------------------------------------------------------------------
-- Colors for windows, bar, windows border etc ---------------------------------
-- Decorative stuff, if you have a different color shceme, change that ---------
-- PRO TIP: change your color scheme to match these colors ---------------------
--------------------------------------------------------------------------------
myNormalBorderColor  = "#7c7c7c"
myFocusedBorderColor = "#55aa55"

tabConfig = def {
    activeBorderColor = "#7C7C7C",
    activeTextColor = "#CEFFAC",
    activeColor = "#000000",
    inactiveBorderColor = "#7C7C7C",
    inactiveTextColor = "#EEEEEE",
    inactiveColor = "#000000"
}

-- xmobar settings
xmobarTitleColor = "#55aa55"
xmobarCurrentWorkspaceColor = "#CEFFAC"
myBorderWidth = 1 -- px



--------------------------------------------------------------------------------
-- ENVIRONMENTAL VARIABLES -----------------------------------------------------
--  You might change it. No you MUST change it to match your preferences -------
--------------------------------------------------------------------------------
myHomePath         = "/home/nox"
myWMName           = "HAL-10000"
myTerminal         = "/usr/bin/kitty"
myTerminalClass    = "Kitty"
myPassmenu         = "keepmenu --clipboard"--"passmenu -nf '#55aa55' -sb '#55aa55'"
myScreensaver      = "/usr/bin/i3lock -c 111111 -e"
myScreenshot       = "flameshot gui"
myLauncher         = "rofi -show drun -terminal kitty -icon-theme 'Papirus' -show-icons -font 'hack 10' -run-shell-command 'kitty -e zsh -ic \"{cmd} && read\"'"
myXmobarrcPath     = "~/.xmonad/.xmobarrc"
myConkyConfigPath  = [myHomePath ++ "/.xmonad/conky.conf", myHomePath ++ "/.xmonad/tasks.conky.conf", myHomePath ++ "/.xmonad/fortune.conky.conf"]
myCalendar         = "calcurse"
myTaskmanager      = "tasksh"
myTrayer           = "trayer"
myTrayerArgs       = "--monitor 1 --edge top --align right --widthtype request --padding 15 --SetDockType true --SetPartialStrut true --expand true --transparent true --alpha 0 --tint 0x222222 --height 15 --distance 0 --margin 350"
myWallpaperChanger = ""
myWebBrowser       = "chromium-browser?"
myNextcloudSync    = "nextcloud"


--------------------------------------------------------------------------------
-- SYSTEM VARIABLES ------------------------------------------------------------
--  You might want to change it according to the apps you're using -------------
--------------------------------------------------------------------------------
audioMute = "pactl set-sink-mute 0 toggle"
audioLow  = "pactl set-sink-volume 0 -5%"
audioHigh = "pactl set-sink-volume 0 +5%"

-- Install playerctl as a dependency
audioPlay = "playerctl play-pause"
audioStop = "playerctl stop"
audioNext = "playerctl next"
audioPrev = "playerctl previous"

shutdown  = "shutdown now"
reboot    = "reboot"

--------------------------------------------------------------------------------
-- Workspaces ------------------------------------------------------------------
-- Example: <action=`xdotool key super+1 button=1>1</action> -------------------
--------------------------------------------------------------------------------
myWorkspacesText :: [[Char]]
myWorkspacesText = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

actionPrefix, actionSuffix :: [Char]
actionPrefix = "<action=`xdotool key super+"
actionSuffix = "</action>"

addActions :: [[Char]] -> [[Char]]
addActions [] = []
addActions [x] = [actionPrefix ++ x ++ "`>" ++ x ++ actionSuffix]
addActions (x:xs) = (actionPrefix ++ x ++ "`>" ++ x ++ actionSuffix) : addActions xs

myWorkspaces :: [[Char]]
myWorkspaces = addActions myWorkspacesText

currentScreen :: X ScreenId
currentScreen = gets (W.screen . W.current . windowset)

isOnScreen :: ScreenId -> WindowSpace -> Bool
isOnScreen s ws = s == unmarshallS (W.tag ws)

workspaceOnCurrentScreen :: WSType
workspaceOnCurrentScreen = WSIs $ do
  s <- currentScreen
  return $ \x -> W.tag x /= "NSP" && isOnScreen s x

switchScreen :: Int -> X ()
switchScreen d = do s <- screenBy d
                    mws <- screenWorkspace s
                    warpToScreen s 0.618 0.618
                    case mws of
                         Nothing -> return ()
                         Just ws -> windows (W.view ws)

--------------------------------------------------------------------------------
-- Keybindings -----------------------------------------------------------------
--------------------------------------------------------------------------------
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  -------------------------
  -- Personal keybindings -
  -------------------------
  [ ((modMask, xK_Return),
     spawn $ XMonad.terminal conf)

  -- Lock the screen using command specified by myScreensaver.
  , ((modMask .|. controlMask, xK_l),
     spawn myScreensaver)

  -- Spawn the launcher using command specified by myLauncher.
  -- Use this to launch programs without a key binding.
  , ((modMask .|. shiftMask, xK_Return),
     spawn myLauncher)

  -- Take a selective screenshot using the command specified by myScreenshot.
   , ((0, xK_Print),
      spawn myScreenshot)

  , ((modMask, xK_f),
     spawn myWebBrowser)

  , ((modMask .|. shiftMask, xK_c),
     spawn (myTerminal ++ " -e " ++ myCalendar))

  , ((modMask .|. shiftMask, xK_t),
     spawn (myTerminal ++ " -e " ++ myTaskmanager))

  , ((modMask .|. shiftMask, xK_a),
     spawn (myTerminal ++ " -e " ++ "atop"))

  , ((modMask .|. shiftMask, xK_h),
     spawn (myTerminal ++ " -e " ++ "htop"))

  , ((modMask .|. shiftMask, xK_i),
     spawn (myTerminal ++ " -e " ++ "insect"))

  , ((modMask .|. controlMask, xK_p),
     spawn myPassmenu)


  ----------------------
  -- Audio keybindings -
  ----------------------
  -- Mute volume.
  , ((0, xF86XK_AudioMute),
     spawn audioMute)

  -- Decrease volume.
  , ((0, xF86XK_AudioLowerVolume),
     spawn audioLow)

  -- Increase volume.
  , ((0, xF86XK_AudioRaiseVolume),
     spawn audioHigh)

  -- Mute volume.
  , ((modMask .|. controlMask, xK_m),
     spawn audioMute)

  -- Decrease volume.
  , ((modMask .|. controlMask, xK_j),
     spawn audioLow)

  -- Increase volume.
  , ((modMask .|. controlMask, xK_k),
     spawn audioHigh)

  -- Audio previous.
  , ((0, 0x1008FF16),
     spawn audioPrev)

  -- Play/pause.
  , ((0, 0x1008FF14),
     spawn audioPlay)

  -- Audio next.
  , ((0, 0x1008FF17),
     spawn audioNext)

  -- Audio stop.
  , ((0, 0x1008FF15),
     spawn audioStop)

  --------------------------------
  -- Windows xmonad key bindings -
  --------------------------------
  -- Close focused window.
  , ((modMask .|. shiftMask, xK_q),
     kill)

  -- Cycle through the available layout algorithms.
  , ((modMask, xK_space),
     sendMessage NextLayout)

  -- Reset the layouts on the current workspace to default.
  , ((modMask .|. shiftMask, xK_space),
     setLayout $ XMonad.layoutHook conf)

  -- Resize viewed windows to the correct size.
  , ((modMask, xK_n),
     refresh)

  -- Move focus to the next window.
  , ((modMask, xK_Tab),
     windows W.focusDown)

  -- Move focus to the next window.
  , ((modMask, xK_j),
     windows W.focusDown)

  , ((modMask, xK_Left),
     windows W.focusDown)

  -- Move focus to the previous window.
  , ((modMask, xK_k),
     windows W.focusUp)

  , ((modMask, xK_Right),
     windows W.focusUp)

  -- Move focus to the master window.
  , ((modMask, xK_m),
     windows W.focusMaster)

  -- Swap the focused window and the master window.
  , ((modMask .|. controlMask, xK_Return),
     windows W.swapMaster)

  -- Swap the focused window with the next window.
  , ((modMask .|. shiftMask, xK_j),
     windows W.swapDown)

  , ((modMask .|. shiftMask, xK_Left),
     windows W.swapDown)

  -- Swap the focused window with the previous window.
  , ((modMask .|. shiftMask, xK_k),
     windows W.swapUp)

  , ((modMask .|. shiftMask, xK_Right),
     windows W.swapUp)

  -- Shrink the master area.
  , ((modMask, xK_h),
     sendMessage Shrink)

  , ((modMask, xK_Down),
     sendMessage Shrink)

  -- Expand the master area.
  , ((modMask, xK_l),
     sendMessage Expand)

  , ((modMask, xK_Up),
     sendMessage Expand)

  -- Push window back into tiling.
  , ((modMask, xK_t),
     withFocused $ windows . W.sink)

  -- Increment the number of windows in the master area.
  , ((modMask, xK_comma),
     sendMessage (IncMasterN 1))

  -- Decrement the number of windows in the master area.
  , ((modMask, xK_period),
     sendMessage (IncMasterN (-1)))

  , ((modMask .|. shiftMask, xK_comma),
    switchScreen 1)

  , ((modMask .|. shiftMask, xK_period),
    shiftNextScreen)

  -- Toggle the status bar gap.
  -- TODO: update this binding with avoidStruts, ((modMask, xK_b),

  -- Quit xmonad.
  , ((modMask .|. shiftMask .|. controlMask, xK_e),
     io exitSuccess)

  -- Restart xmonad.
  , ((modMask .|. shiftMask .|. controlMask, xK_r),
     restart "xmonad" True)

  -- Change wallpaper
  , ((modMask .|. controlMask, xK_w),
     spawn myWallpaperChanger)

  -- Reboot.
  , ((modMask .|. controlMask, xK_x),
     spawn shutdown)

  -- Shutdown.
  , ((modMask .|. controlMask, xK_r),
     spawn reboot)
  ]
  ++
  -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
  [((m .|. modMask, k), windows $ onCurrentScreen f i)
      | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig {XMonad.modMask = modm} = M.fromList
  [ ((modm, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
  , ((modm .|. shiftMask, button1), dragWindow)
  , ((modm, button2), const kill)
  , ((modm, button3), \w -> focus w >> Flex.mouseResizeWindow w)
  , ((modm, button4), \_ -> moveTo Prev workspaceOnCurrentScreen)
  , ((modm, button5), \_ -> moveTo Next workspaceOnCurrentScreen)
  ]

--------------------------------------------------------------------------------
-- Layouts ---------------------------------------------------------------------
--------------------------------------------------------------------------------
myLayoutHook = avoidStruts (
   ThreeColMid 1 (3/100) (1/2) |||
   Mirror (Tall 1 (3/100) (1/2)) |||
   tabbed shrinkText tabConfig |||
   noBorders (fullscreenFull Full) |||
   spiral (6/7))


--------------------------------------------------------------------------------
-- Magifuckery -----------------------------------------------------------------
--------------------------------------------------------------------------------
(~?) :: Eq a => Query [a] -> [a] -> Query Bool
q ~? x = fmap (x `L.isInfixOf`) q

(/=?) :: Eq a => Query a -> a -> Query Bool
q /=? x = fmap (/= x) q

myHandleEventHook :: Event -> X All
myHandleEventHook = multiScreenFocusHook

screenCount :: X Int
screenCount = withDisplay (io.fmap length.getScreenInfo)

newtype MyUpdatePointerActive = MyUpdatePointerActive Bool
instance ExtensionClass MyUpdatePointerActive where
   initialValue = MyUpdatePointerActive True

myUpdatePointer :: (Rational, Rational) -> (Rational, Rational) -> X ()
myUpdatePointer refPos ratio =
  whenX isActive $ do
    dpy <- asks display
    root <- asks theRoot
    (_,_,_,_,_,_,_,m) <- io $ queryPointer dpy root
    unless (testBit m 9 || testBit m 8 || testBit m 10) $ -- unless the mouse is clicking
      updatePointer refPos ratio

  where
    isActive = (\(MyUpdatePointerActive b) -> b) <$> XS.get

multiScreenFocusHook :: Event -> X All
multiScreenFocusHook MotionEvent { ev_x = x, ev_y = y } = do
  ms <- getScreenForPos x y
  case ms of
    Just cursorScreen -> do
      let cursorScreenID = W.screen cursorScreen
      focussedScreenID <- gets (W.screen . W.current . windowset)
      when (cursorScreenID /= focussedScreenID) (focusWS $ W.tag $ W.workspace cursorScreen)
      return (All True)
    _ -> return (All True)
  where getScreenForPos :: CInt -> CInt
            -> X (Maybe (W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail))
        getScreenForPos x y = do
          ws <- windowset <$> get
          let screens = W.current ws : W.visible ws
              inRects = map (inRect x y . screenRect . W.screenDetail) screens
          return $ fst <$> find snd (zip screens inRects)
        inRect :: CInt -> CInt -> Rectangle -> Bool
        inRect x y rect = let l = fromIntegral (rect_x rect)
                              r = l + fromIntegral (rect_width rect)
                              t = fromIntegral (rect_y rect)
                              b = t + fromIntegral (rect_height rect)
                           in x >= l && x < r && y >= t && y < b
        focusWS :: WorkspaceId -> X ()
        focusWS ids = windows (W.view ids)
multiScreenFocusHook _ = return (All True)


--------------------------------------------------------------------------------
-- How the individual windows are shown ----------------------------------------
--------------------------------------------------------------------------------
myManageHook :: ManageHook
myManageHook = composeAll
  [ resource  =? "desktop_window" --> doIgnore
  , isFloat --> doCenterFloat
  , isDialog --> doCenterFloat
  , title =? "Tor Browser" --> doFloat
  , className =? "steam_app_0" --> doFloat
  , className =? "noita.exe" --> doFloat
  , appName =? "AIMP" -->doFloat
  , appName =? "noita.exe" --> doFloat
  , insertPosition Master Newer
  ] <+> manageDocks


--------------------------------------------------------------------------------
-- Status/Top Bar --------------------------------------------------------------
--------------------------------------------------------------------------------
myStatusBarSpawner :: Applicative f => ScreenId -> f StatusBarConfig
myStatusBarSpawner (S s) = do
  pure $ statusBarPropTo ("_XMONAD_LOG_" ++ show s)
        ("xmobar -x " ++ show s ++ " ~/.xmonad/.xmobarrc" ++ show s)
        (pure $ myXmobarPP (S s))


myXmobarPP :: ScreenId -> PP
myXmobarPP s  = marshallPP s $ def
  { ppTitle = xmobarColor xmobarTitleColor "" . shorten 70
  , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
  , ppSep = "   "
  , ppOrder = \(ws : _ : _ : extras) -> ws : extras
  , ppExtras  = [ logLayoutOnScreen s
                , titleColorIsActive s (shortenL (if s == 0 then 75 else 75) $ logTitleOnScreen s)
                ]
  }
   where
    titleColorIsActive n l = do
      c <- withWindowSet $ return . W.screen . W.current
      if n == c then xmobarColorL xmobarTitleColor  "" l else xmobarColorL myFocusedBorderColor "" l

--------------------------------------------------------------------------------
-- Conky (on-screen customizable info) -----------------------------------------
--------------------------------------------------------------------------------
myConkySetup :: [String] -> X ()
myConkySetup [] = return ()
myConkySetup [x] = do spawn ("conky --pause=2 -c " ++ x)
myConkySetup (x:xs) = do
   spawn ("conky --pause=2 -c " ++ x)
   myConkySetup xs

--------------------------------------------------------------------------------
-- What apps should load on login/restart --------------------------------------
--------------------------------------------------------------------------------
myStartupHook :: X ()
myStartupHook = do
   spawnOnce $ "setxkbmap -layout us,ru,ro -option grp:caps_toggle -variant ,,std"
   spawn $ "bash ~/.xmonad/.screenlayout/HAL-10000-1.sh"
   spawn $ "nitrogen --restore"
   spawnOnce $ "xcompmgr &"
   spawnOnce $ myTrayer ++ " " ++ myTrayerArgs
   spawnOnce $ "conky -c /home/nox/.xmonad/conky.conf & conky -c /home/nox/.xmonad/fortune.conky.conf & conky -c /home/nox/.xmonad/tasks.conky.conf &"
   spawnOnce $ "birdtray &"
   spawnOnce $ "kdeconnect-indicator &"
   -- spawnOnce $ "syncthingtray &"
   spawnOnce $ "twmnd &"
   spawnOnce $ myNextcloudSync
   spawnOnce $ "fcitx5 &"
   spawnOnce $ "numlockx &"
   -- myConkySetup myConkyConfigPath
   modify $ \xstate -> xstate { windowset = onlyOnScreen 0 "0_1" (windowset xstate) }
   modify $ \xstate -> xstate { windowset = onlyOnScreen 1 "1_1" (windowset xstate) }
   -- For java compatibility
   --  [see this bug](https://wiki.archlinux.org/title/Java#Gray_window,_applications_not_resizing_with_WM,_menus_immediately_closing)
   setWMName "LG3D"
   

--------------------------------------------------------------------------------
-- EXECUTEEEEE -----------------------------------------------------------------
--------------------------------------------------------------------------------
main :: IO ()
main = xmonad
     . ewmhFullscreen
     . setEwmhActivateHook doAskUrgent
     . ewmh
     . dynamicSBs myStatusBarSpawner
     . docks
     $ def
       { focusFollowsMouse  = True
       , clickJustFocuses   = False
       , borderWidth        = myBorderWidth
       , modMask            = mod4Mask
       , normalBorderColor  = myNormalBorderColor
       , focusedBorderColor = myFocusedBorderColor
       , terminal           = myTerminal
       , keys               = myKeys
       , workspaces         = withScreens 2 myWorkspaces
       , mouseBindings      = myMouseBindings
       , layoutHook         = myLayoutHook
       , manageHook         = myManageHook
       , startupHook        = myStartupHook
       , rootMask           = rootMask def .|. pointerMotionMask
       , handleEventHook    = myHandleEventHook
       }
