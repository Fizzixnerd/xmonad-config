{-# LANGUAGE   OverloadedStrings
             , FlexibleContexts
             , FlexibleInstances #-}

module XMonad.Hooks.DynamicLog.Dzen2.Format where

import qualified Data.List as L
import Data.String

import XMonad.Util.Font
import XMonad hiding (Status, Color, Font)
import XMonad.Hooks.DynamicLog.Dzen2.Width
import XMonad.Hooks.DynamicLog.Dzen2.Status
import XMonad.Hooks.DynamicLog.Dzen2.StatusText
import XMonad.Hooks.DynamicLog.Dzen2.Font

instance IsString (StatusText fnt) where
  fromString = mkStatusText

mkStatusT :: s -> StatusT fnt s
mkStatusT = return

mkStatusText :: String -> StatusText fnt
mkStatusText = mkStatusT

move :: RelativePosition -> StatusT fnt s -> StatusT fnt s
move rp = onto $ SP rp

put :: AbsolutePosition -> StatusT fnt s -> StatusT fnt s
put ap = onto $ SPA ap

fg :: Color -> StatusT fnt s -> StatusT fnt s
fg c = onto $ SFG c

bg :: Color -> StatusT fnt s -> StatusT fnt s
bg c = onto $ SBG c

intersperseST :: StatusT fnt s -> [StatusT fnt s] -> StatusT fnt s
intersperseST sep xs = StatusT $ SList $ fmap unpack $ L.intersperse sep xs

atLeft' :: StatusT fnt s -> StatusT fnt s
atLeft' = move RPLeft

atLeft :: StatusT fnt s -> X (StatusT fnt s)
atLeft = return . atLeft'

atRight' :: StatusT fnt s -> StatusT fnt s
atRight' = move RPRight

atRight :: Width (StatusT fnt s) X => StatusT fnt s -> X (StatusT fnt s)
atRight st = do
  w <- width st
  return $ atRight' $ move (RPPosition $ PX $ -w) st

atCenter' :: StatusT fnt s -> StatusT fnt s
atCenter' = move RPCenter
  
atCenter :: Width (StatusT fnt s) X => StatusT fnt s -> X (StatusT fnt s)
atCenter st = do
  w <- width st
  return $ atCenter' $ move (RPPosition $ PX $ -w `div` 2) st
