{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module XMonad.Hooks.DynamicLog.Status.Bars where

import XMonad
import XMonad.Layout.LayoutModifier
import XMonad.Util.Run
import qualified XMonad.Hooks.DynamicLog.Status.StatusText as ST
import XMonad.Hooks.ManageDocks
import qualified XMonad.Hooks.DynamicLog.Status.DZen2.Universal as U

import Control.Monad
import Control.Monad.IO.Class

import qualified Data.Map as M
import qualified Data.List as L
import qualified System.IO as IO
import Data.Monoid

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

newtype StatusBarSection = StatusBarSection [ST.StatusText]

-- | Left, middle, and right sections of the StatusBar respectively, and a separator
newtype StatusBar = StatusBar (StatusBarSection, StatusBarSection, StatusBarSection)

makeStatusBarSection :: [ST.StatusText] -> StatusBarSection
makeStatusBarSection sts = StatusBarSection sts

makeStatusBar :: StatusBarSection -> StatusBarSection -> StatusBarSection -> StatusBar
makeStatusBar l c r = StatusBar (l, c, r)

-- | Return the logical length of the StatusBarSection, without the
-- control characters.
length :: StatusBarSection -> Int
length (StatusBarSection xs) = sum $ fmap ST.length $ xs

-- | Return the number of different StatusTexts inside the
-- StatusBarSection.
numberOfSections :: StatusBarSection -> Int
numberOfSections (StatusBarSection xs) = L.length xs

-- | FIXME: This is undefined.
numberOfNonemptySections :: StatusBarSection -> Int
numberOfNonemptySections (StatusBarSection xs) = undefined

left :: StatusBar -> StatusBarSection
left (StatusBar (l,c,r)) = l

center :: StatusBar -> StatusBarSection
center (StatusBar (l,c,r)) = c

right :: StatusBar -> StatusBarSection
right (StatusBar (l,c,r)) = r

simpleRenderStatusBarSection :: StatusBarSection -> T.Text -> T.Text
simpleRenderStatusBarSection (StatusBarSection sbs) sep = mconcat $ fmap ST.render $ L.intersperse (ST.simpleStatusText sep) sbs

simpleRenderBar :: StatusBar -> T.Text -> T.Text
simpleRenderBar sb sep = 
  let l = left sb
      c = center sb
      r = right sb
      leftAlignedL = ST.render $ const (simpleRenderStatusBarSection l sep) <$> U.p U.LEFT U.HERE
      centerAlignedC = ST.render $ const (simpleRenderStatusBarSection c sep) <$> U.p U.CENTER U.HERE
      rightAlignedR = ST.render $ const (simpleRenderStatusBarSection r sep) <$> U.p U.RIGHT U.HERE
  in
    leftAlignedL <> centerAlignedC <> rightAlignedR

defaultRenderBar :: StatusBar -> T.Text
defaultRenderBar bar = simpleRenderBar bar sep
  where sep = " | "

hPrintStatusBar :: IO.Handle -> StatusBar -> (StatusBar -> T.Text) -> X ()
hPrintStatusBar h bar renderF = liftIO $ TIO.hPutStrLn h $ renderF bar

statusBar :: LayoutClass l Window
             => T.Text -- ^ The command line to launch the status bar.
             -> X StatusBar -- ^ The StatusTexts to print
             -> X (StatusBar -> T.Text)
             -> (XConfig Layout -> (KeyMask, KeySym))
             -- ^ The desired key binding to toggle bar visibility.
             -> XConfig l -- ^ The base config.
             -> IO (XConfig (ModifiedLayout AvoidStruts l))
statusBar cmd bar renderF k conf = do
  h <- spawnPipe $ T.unpack cmd
  return $ conf 
    { layoutHook = avoidStruts $ layoutHook conf 
    , logHook    = do
        logHook conf
        theBar <- bar
        theRenderer <- renderF
        hPrintStatusBar h theBar theRenderer
    , manageHook = manageHook conf <+> manageDocks
    , keys       = liftM2 M.union keys' (keys conf)
    }
    where
       keys' = (`M.singleton` sendMessage ToggleStruts) . k

defaultStatusBar :: LayoutClass l Window 
                   => T.Text
                   -> X StatusBar 
                   -> (XConfig Layout -> (KeyMask, KeySym))
                   -> XConfig l
                   -> IO (XConfig (ModifiedLayout AvoidStruts l))
defaultStatusBar cmd bar k conf = statusBar cmd bar (return defaultRenderBar) k conf
