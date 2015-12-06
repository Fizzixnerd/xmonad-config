{-# LANGUAGE FlexibleContexts, DeriveDataTypeable, CPP,
OverloadedStrings #-}

module Main where

import XMonad

import System.Desktop.Commands.Fizzixnerd

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicLog.PrettyPrinter hiding (workspaces)
import XMonad.Hooks.DynamicLog.PrettyPrinter.DZen2
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.Fizzixnerd

import XMonad.Actions.UpdateFocus
import XMonad.Actions.Fizzixnerd

import XMonad.Layout.SimpleDecoration
import XMonad.Layout.ImageButtonDecoration
import XMonad.Layout.WindowSwitcherDecoration
import XMonad.Layout.DraggingVisualizer

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import XMonad.Util.Loggers hiding (battery)
import XMonad.Util.Timer
import qualified XMonad.Util.ExtensibleState as XS
import qualified XMonad.Util.Dzen as Dzen

import System.IO

import System.Information.Battery

-- WORKSPACES

myWorkspaces = ["1:Main", "2:Web", "3:Steam", "4:Media", "5:Terminal", "6", "7", "8", "9"]

-- KEYS

myGenericKeys = [ ("M-e", runExplorer)
                , ("M-b", runBrowser)
                , ("M-m", runMusic)
                , ("M-v", runVideo)
                , ("M-t", runTerminal)
                , ("M-s", runSystemMonitor)
                ]

myMultimediaKeys = [ ("<XF86AudioMute>", runToggleMute)
                   , ("<XF86AudioRaiseVolume>", runVolumeUp)
                   , ("<XF86AudioLowerVolume>", runVolumeDown)
                   ]

myKeys = concat [myGenericKeys, myMultimediaKeys]

-- CONFIG

myConfig = ewmh $ defaultConfig
  { modMask = mod4Mask
  , terminal = myTerminal
  , workspaces = myWorkspaces
  , startupHook = myStartupHook
  , logHook = myLogHook
  , manageHook = myManageHook
  , layoutHook = myLayoutHook
  , handleEventHook = myhandleEventHook
  } `additionalKeysP` myKeys

-- DZEN2

-- | This is horrible, and needs to be fixed.  Do that.
-- UPDATE: Okay, I've made a library that should make this better.
-- order :: [String] -> [String]
-- order (ws:lo:cwt:extras) = "^p(_LEFT)":
--                            ws:
--                            ("^p(_CENTER)^p(-" ++ show (18 * (length (dzenStrip $ lo ++ cwt)) `div` 2) ++ ")"):
--                            lo:
--                            cwt:
--                            ("^p(_RIGHT)^p(-" ++ show (11 * (length $ dzenStrip $ concat extras)) ++ ")"):
--                            extras

dzen2Time :: Logger
dzen2Time = dzenColorL "green" "black" $ logCmd time

-- dzen2PP = defaultPP { ppTitle = const ""
--                     , ppExtras = [dzen2Time, battery]
--                     , ppOrder = order
--                     , ppSep = " "
--                     , ppLayout = last . words
--                     }

dzen2 config = do
  b <- battery
  dynamicStatusBar "dzen2 -fn 'Droid Sans Mono:style=Regular' -ta c -w 1920" b (const (mod4Mask, xK_P)) config

-- MAIN

main = do
  xmonad =<< dzen2 myConfig
