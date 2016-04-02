{-# LANGUAGE FlexibleContexts #-}

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

-- | A bunch of StatusTexts that belong together in the same area of
-- the StatusBar.
newtype StatusBarSection = StatusBarSection [ST.StatusText]

-- | Left, center, and right sections of the StatusBar respectively.
newtype StatusBar = StatusBar (StatusBarSection, StatusBarSection, StatusBarSection)

makeStatusBarSection :: [ST.StatusText] -> StatusBarSection
makeStatusBarSection sbs = StatusBarSection sbs

makeStatusBar :: StatusBarSection -> StatusBarSection -> StatusBarSection -> StatusBar
makeStatusBar l c r = StatusBar (l, c, r)

-- | Return the logical length of the StatusBarSection, without the
-- control characters.
length :: StatusBarSection -> Int
length (StatusBarSection xs) = sum $ fmap ST.length $ xs

-- | Return the number of different StatusTexts inside the
-- StatusBarSection.
numberOfTexts :: StatusBarSection -> Int
numberOfTexts (StatusBarSection xs) = L.length xs

-- | Return the number of non-empty StatusTexts inside.
numberOfNonemptyTexts :: StatusBarSection -> Int
numberOfNonemptyTexts (StatusBarSection xs) = L.length $ filter ST.isEmpty xs

-- | Return the left section of the StatusBar.
left :: StatusBar -> StatusBarSection
left (StatusBar (l,c,r)) = l

-- | Return the center section of the StatusBar.
center :: StatusBar -> StatusBarSection
center (StatusBar (l,c,r)) = c

-- | Return the right section of the StatusBar.
right :: StatusBar -> StatusBarSection
right (StatusBar (l,c,r)) = r

-- | Return the rendered StatusBarSection with StatusTexts separated by the
-- String sep.
simpleRenderStatusBarSection :: StatusBarSection -> String -> String
simpleRenderStatusBarSection (StatusBarSection sbs) sep = mconcat $ fmap ST.render $ L.intersperse (ST.simpleStatusText sep) sbs

-- | TODO: Fix the placement of the text.
simpleRenderBar :: StatusBar -> String -> String
simpleRenderBar sb sep = 
  let l = left sb
      c = center sb
      r = right sb
      leftAlignedL   = ST.render $ const (simpleRenderStatusBarSection l sep) <$> U.p U.LEFT   U.HERE
      centerAlignedC = ST.render $ const (simpleRenderStatusBarSection c sep) <$> U.p U.CENTER U.HERE
      rightAlignedR  = ST.render $ const (simpleRenderStatusBarSection r sep) <$> U.p U.RIGHT  U.HERE
  in
    leftAlignedL <> centerAlignedC <> rightAlignedR

defaultRenderBar :: StatusBar -> String
defaultRenderBar bar = simpleRenderBar bar sep
  where 
    sep = " | "

hPrintStatusBar :: IO.Handle -> StatusBar -> (StatusBar -> String) -> X ()
hPrintStatusBar h bar renderF = liftIO $ hPutStrLn h $ renderF bar

statusBar :: LayoutClass l Window
             => String -- ^ The command line to launch the status bar.
             -> X StatusBar -- ^ The StatusTexts to print
             -> X (StatusBar -> String)
             -> (XConfig Layout -> (KeyMask, KeySym))
             -- ^ The desired key binding to toggle bar visibility.
             -> XConfig l -- ^ The base config.
             -> IO (XConfig (ModifiedLayout AvoidStruts l))
statusBar cmd bar renderF k conf = do
  h <- spawnPipe cmd
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
                    => String
                    -> X StatusBar 
                    -> (XConfig Layout -> (KeyMask, KeySym))
                    -> XConfig l
                    -> IO (XConfig (ModifiedLayout AvoidStruts l))
defaultStatusBar cmd bar k conf = statusBar cmd bar (return defaultRenderBar) k conf
