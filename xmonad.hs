{-# LANGUAGE FlexibleContexts,
             FlexibleInstances,
             DeriveDataTypeable,
             OverloadedStrings,
             TypeSynonymInstances #-}

module Main where

import XMonad hiding (Font)

import Data.Monoid

import System.Desktop.Commands.Fizzixnerd

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.Fizzixnerd
-- import XMonad.Hooks.DynamicLog.Status.X
-- import XMonad.Hooks.DynamicLog.Status.StatusText
-- import XMonad.Hooks.DynamicLog.Status.DZen2.Universal
-- import XMonad.Hooks.DynamicLog.Status.DZen2.Fancy
-- import XMonad.Hooks.DynamicLog.Status.System
import qualified XMonad.Hooks.DynamicLog.Dzen2.Bars as B
import qualified XMonad.Hooks.DynamicLog.Dzen2.Placement as P
import qualified XMonad.Hooks.DynamicLog.Dzen2.StatusText as ST
import XMonad.Hooks.DynamicLog.Dzen2.Status
import XMonad.Hooks.DynamicLog.Dzen2.System
import XMonad.Hooks.DynamicLog.Dzen2.X
import XMonad.Hooks.DynamicLog.Dzen2.Width
import XMonad.Hooks.DynamicLog.Dzen2.Font

import XMonad.Actions.Fizzixnerd

import XMonad.Layout.LayoutModifier

import XMonad.Util.EZConfig
import XMonad.Util.Timer
import XMonad.Util.Font


-- WORKSPACES


myWorkspaces :: [String]
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
  , XMonad.workspaces = myWorkspaces
  , startupHook = myStartupHook
  , logHook = myLogHook
  , manageHook = myManageHook
  , layoutHook = myLayoutHook
  , handleEventHook = myhandleEventHook
  } `additionalKeysP` myKeys

-- FONT

partialFontName :: FontName
partialFontName = FontName "Droid Sans Mono-12"

myFontName :: FontName
myFontName = FontName "Droid Sans Mono-12:style=Regular"

myFont :: X XMonadFont
myFont = case myFontName of 
  (FontName mfn) -> initXMF $ "xft: " <> mfn

-- DZEN2
-- dzen2 :: LayoutClass l Window => 
--          XConfig l ->
--          IO (XConfig (ModifiedLayout AvoidStruts l))
-- dzen2 conf = do
--   let myStatusBar = do 
--         cb <- coloredBattery
--         d <- date
--         slt <- simpleLayoutTitle
--         dwt <- defaultWindowTitle
-- --        dws <- defaultWorkspaces
--         return $ B.makeStatusBar (B.makeStatusBarSection [cb, d >>= (fg "red"), slt, dwt]) (B.makeStatusBarSection []) (B.makeStatusBarSection [])
--   B.defaultStatusBar ("dzen2 -fn " <> myFontName <> " -ta c -w 1920") myStatusBar (const (mod4Mask, xK_P)) conf

data MyFont
instance Font (ST.StatusText MyFont) where
  font = const myFont

dzen2 :: LayoutClass l Window =>
         FontName ->
         FontName ->
         XConfig l ->
         IO (XConfig (ModifiedLayout AvoidStruts l))
dzen2 pfn (FontName fontName) conf = do
  let myStatusBar = do
        date' <- date
        battery' <- battery
        slt <- simpleLayoutTitle
        dwt <- defaultWindowTitle
        return $ B.mkStatusBar date' (slt <> (P.mkStatusText " | ") <> dwt)  battery'
  B.defaultStatusBar ("dzen2 -fn " <> fontName <> " -ta c -w 1920") (myStatusBar :: X (B.StatusBar MyFont)) (const (mod4Mask, xK_P)) conf pfn

-- MAIN
main :: IO ()
main = do
  case myFontName of
    (FontName mfn) ->
      xmonad =<< dzen2 partialFontName (FontName ("'" <> mfn <> "'")) myConfig
