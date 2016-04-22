{-# LANGUAGE FlexibleContexts #-}

module XMonad.Hooks.DynamicLog.Dzen2.Bars where

import XMonad hiding (Font)
import XMonad.Layout.LayoutModifier
import XMonad.Util.Run
import XMonad.Util.Font
import XMonad.Hooks.DynamicLog.Dzen2.Font
import XMonad.Hooks.DynamicLog.Dzen2.Render
import XMonad.Hooks.DynamicLog.Dzen2.Content
import qualified XMonad.Hooks.DynamicLog.Dzen2.StatusText as ST
import qualified XMonad.Hooks.DynamicLog.Dzen2.Format as F
import XMonad.Hooks.ManageDocks

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Map as M
import qualified Data.List as L
import qualified System.IO as IO
import Data.Monoid

newtype StatusBar fnt = StatusBar (ST.StatusText fnt, ST.StatusText fnt, ST.StatusText fnt)

mkStatusBar :: ST.StatusText fnt -> ST.StatusText fnt -> ST.StatusText fnt -> StatusBar fnt
mkStatusBar l c r = StatusBar (l, c, r)

-- | Return the left section of the StatusBar.
left :: StatusBar fnt -> ST.StatusText fnt
left (StatusBar (l,c,r)) = l

-- | Return the center section of the StatusBar.
center :: StatusBar fnt -> ST.StatusText fnt
center (StatusBar (l,c,r)) = c

-- | Return the right section of the StatusBar.
right :: StatusBar fnt -> ST.StatusText fnt
right (StatusBar (l,c,r)) = r

-- | TODO: Fix the placement of the text.
simpleRenderBar :: Font (ST.StatusText fnt) => StatusBar fnt -> ST.StatusText fnt -> X String
simpleRenderBar sb sep = 
  let l = left sb
      c = center sb
      r = right sb
      leftAlignedL   = render <$> F.atLeft l
      centerAlignedC = render <$> F.atCenter c
      rightAlignedR  = render <$> F.atRight r
  in
    do
      lal <- leftAlignedL
      cac <- centerAlignedC 
      rar <- rightAlignedR
      return $ lal <> cac <> rar

defaultRenderBar :: Font (ST.StatusText fnt) => StatusBar fnt -> X String
defaultRenderBar bar = do
  simpleRenderBar bar $ F.mkStatusText " | "

hPrintStatusBar :: IO.Handle -> StatusBar fnt -> (StatusBar fnt -> X String) -> X ()
hPrintStatusBar h bar renderF = do
  s <- renderF bar
  liftIO $ hPutStrLn h s

statusBar :: LayoutClass l Window
             => String -- ^ The command line to launch the status bar.
             -> X (StatusBar fnt) -- ^ The ST.StatusTexts to print
             -> (StatusBar fnt -> X String)
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
        hPrintStatusBar h theBar renderF
    , manageHook = manageHook conf <+> manageDocks
    , keys       = liftM2 M.union keys' (keys conf)
    }
    where
       keys' = (`M.singleton` sendMessage ToggleStruts) . k

defaultStatusBar :: (LayoutClass l Window
                    ,Font (ST.StatusText fnt))
                    => String
                    -> X (StatusBar fnt)
                    -> (XConfig Layout -> (KeyMask, KeySym))
                    -> XConfig l
                    -> FontName
                    -> IO (XConfig (ModifiedLayout AvoidStruts l))
defaultStatusBar cmd bar k conf fn = statusBar cmd bar defaultRenderBar k conf
