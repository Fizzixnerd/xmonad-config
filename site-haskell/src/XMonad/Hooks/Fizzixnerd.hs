module XMonad.Hooks.Fizzixnerd where

import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.EwmhDesktops

import XMonad.Layout.BoringWindows
import XMonad.Layout.Minimize
import XMonad.Layout.Maximize
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.ImageButtonDecoration
import XMonad.Layout.WindowSwitcherDecoration
import XMonad.Layout.DraggingVisualizer

import XMonad.Util.Timer
import qualified XMonad.Util.ExtensibleState as XS

import XMonad.Actions.UpdateFocus
import XMonad.Actions.Fizzixnerd

import Data.Monoid

-- TIMERHOOK
 -- From: http://stackoverflow.com/questions/11045239/can-xmonads-loghook-be-run-at-set-intervals-rather-than-in-merely-response-to

data TidState = TID TimerId deriving Typeable

instance ExtensionClass TidState where
  initialValue = TID 0

clockStartupHook = startTimer 1 >>= XS.put . TID

clockEventHook e = do
  (TID t) <- XS.get
  handleTimer t e $ do
    startTimer 1 >>= XS.put . TID
    ask >>= logHook.config
    return Nothing
  return $ All True

-- STARTUPHOOk

myStartupHook = runCompton 
                >> waitHalfSecond 
                >> runGnomeDo 
                >> runDock 
                >> adjustEventInput 
                -- >> clockStartupHook

-- WINDOW RULES

ignoreGnomeDo = className =? "Do" --> doIgnore
ignoreCairoDock = className =? "Cairo-dock" --> doIgnore
ignoreDocky = className =? "Docky" --> doIgnore
floatNMConnectionEditor = className =? "Nm-connection-editor" --> doFloat
fullscreenFullscreen = isFullscreen --> doFullFloat

-- LOGHOOK

myLogHook = fadeInactiveLogHook 0.8

-- LAYOUTHOOK

myLayoutHook = boringAuto 
               $ maximize 
               $ minimize 
               $ windowSwitcherDecorationWithImageButtons (shrinkText) defaultThemeWithImageButtons 
               $ draggingVisualizer 
               $ layoutHook defaultConfig

-- MANAGEHOOK

myManageHook = manageDocks 
               <+> ignoreGnomeDo 
               <+> ignoreCairoDock 
               <+> ignoreDocky 
               <+> floatNMConnectionEditor 
               <+> fullscreenFullscreen

-- HANDLEEVENTHOOK

myhandleEventHook = docksEventHook 
                    <+> focusOnMouseMove 
                    <+> clockEventHook
