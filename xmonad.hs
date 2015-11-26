{-# LANGUAGE FlexibleContexts #-}

import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.EwmhDesktops

import XMonad.Actions.UpdateFocus

import XMonad.Layout.SimpleDecoration
import XMonad.Layout.ImageButtonDecoration
import XMonad.Layout.WindowSwitcherDecoration
import XMonad.Layout.DraggingVisualizer

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import qualified XMonad.Util.Dzen as Dzen

import System.IO

-- SHELL COMMANDS

browser = "firefox"
explorer = "nautilus"
systemMonitor = "gnome-system-monitor"
music = "rhythmbox"
video = "vlc"
gnomeDo = "gnome-do"
myTerminal = "gnome-terminal"
dock' = "killall cairo-dock; sleep 0.5; cairo-dock"
dock = "killall docky; sleep 0.5; docky"
volumeUp = "amixer -D pulse sset Master 5%+"
volumeDown = "amixer -D pulse sset Master 5%-"
toggleMute = "amixer -D pulse set Master 1+ toggle"
compton = "killall compton; sleep 0.5; compton -f -I 0.10 -O 0.10 --backend glx --vsync opengl"
time = "date"
myBattery = "acpi"

conkyTimeString = "^fg(\\#ff0000)${time %a %b %d %I:%M%P}^fg()"

-- COMMAND ACTIONS

waitHalfSecond :: X ()
waitHalfSecond = do
  spawn "sleep 0.5"

runBrowser :: X ()
runBrowser = do
  spawn browser

runExplorer :: X ()
runExplorer = do
  spawn explorer

runSystemMonitor :: X ()
runSystemMonitor = do
  spawn systemMonitor

runVideo :: X ()
runVideo = do
  spawn video

runMusic :: X ()
runMusic = do
  spawn music

runTerminal :: X ()
runTerminal = do
  spawn myTerminal

runCompton :: X ()
runCompton = do
  spawn compton

runGnomeDo :: X ()
runGnomeDo = do
  spawn gnomeDo

runDock :: X ()
runDock = do
  spawn dock

runVolumeUp :: X ()
runVolumeUp = do
  spawn volumeUp

runVolumeDown :: X ()
runVolumeDown = do
  spawn volumeDown

runToggleMute :: X ()
runToggleMute = do
  spawn toggleMute

-- STARTUP

myStartupHook = runCompton >> waitHalfSecond >> runGnomeDo >> runDock >> adjustEventInput

-- WINDOW RULES

ignoreGnomeDo = className =? "Do" --> doIgnore
ignoreCairoDock = className =? "Cairo-dock" --> doIgnore
ignoreDocky = className =? "Docky" --> doIgnore
floatNMConnectionEditor = className =? "Nm-connection-editor" --> doFloat
fullscreenFullscreen = isFullscreen --> doFullFloat

-- WORKSPACES

myWorkspaces = ["1:Main", "2:Web", "3:Steam", "4:Media", "5:Terminal", "6", "7", "8", "9"]

-- KEYS

myGenericKeys = [("M-e", runExplorer)
                ,("M-b", runBrowser)
                ,("M-m", runMusic)
                ,("M-v", runVideo)
                ,("M-t", runTerminal)
                ,("M-s", runSystemMonitor)]

myMultimediaKeys = [("<XF86AudioMute>", runToggleMute)
                   ,("<XF86AudioRaiseVolume>", runVolumeUp)
                   ,("<XF86AudioLowerVolume>", runVolumeDown)]

myKeys = concat [myGenericKeys, myMultimediaKeys]

-- LOGHOOK

myLogHook = fadeInactiveLogHook 0.8

-- LAYOUT

myLayout = windowSwitcherDecorationWithImageButtons shrinkText defaultThemeWithImageButtons (draggingVisualizer $ layoutHook defaultConfig)

-- CONFIG

myConfig = ewmh $ defaultConfig
  { modMask = mod4Mask
  , terminal = myTerminal
  , workspaces = myWorkspaces
  , startupHook = myStartupHook
  , logHook = myLogHook
  , manageHook = manageDocks <+> ignoreGnomeDo <+> ignoreCairoDock <+> ignoreDocky <+> floatNMConnectionEditor <+> fullscreenFullscreen
  , layoutHook = myLayout
  , handleEventHook = docksEventHook <+> focusOnMouseMove
  } `additionalKeysP` myKeys

-- DZEN2

dzenFont :: String
dzenFont = "Droid Sans Mono:style=Regular"

myDzenConfig = Dzen.font dzenFont

dzen2Time :: Logger
dzen2Time = dzenColorL "green" "black" $ logCmd time

dzen2PP = byorgeyPP {
  ppExtras = [dzen2Time, battery]
}
  
dzen2 config = statusBar "dzen2" dzen2PP (const (mod4Mask, xK_P)) config

-- MAIN

main = do
  xmonad =<< dzen2 myConfig
