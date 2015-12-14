{-# LANGUAGE FlexibleContexts, DeriveDataTypeable,
             OverloadedStrings #-}

module Main where

import XMonad

import System.Desktop.Commands.Fizzixnerd

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.Fizzixnerd
import XMonad.Hooks.DynamicLog.Status.StatusText
import XMonad.Hooks.DynamicLog.Status.DZen2.Universal
import XMonad.Hooks.DynamicLog.Status.DZen2.Fancy
import XMonad.Hooks.DynamicLog.Status.System
import qualified XMonad.Hooks.DynamicLog.Status.Bars as B

import XMonad.Actions.Fizzixnerd

import XMonad.Layout.LayoutModifier

import XMonad.Util.EZConfig
import XMonad.Util.Loggers hiding (battery, date)
import XMonad.Util.Timer

import Text.PrettyPrint

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
dzen2Time :: Logger
dzen2Time = dzenColorL "green" "black" $ logCmd time

dzen2 :: LayoutClass l Window => 
         XConfig l ->
         IO (XConfig (ModifiedLayout AvoidStruts l))
dzen2 conf = do
  let myStatusBar = do 
        cb <- coloredBattery
        d <- date
        return $ B.makeStatusBar (B.makeStatusBarSection [cb, d >>= (fg "red")]) (B.makeStatusBarSection []) (B.makeStatusBarSection [])
  B.defaultStatusBar "dzen2 -fn 'Droid Sans Mono:style=Regular' -ta c -w 1920" myStatusBar (const (mod4Mask, xK_P)) conf

 -- (sepBy (text " | ") [coloredBattery, (fg "red") <$> date])
-- MAIN
main = do
  xmonad =<< dzen2 myConfig
