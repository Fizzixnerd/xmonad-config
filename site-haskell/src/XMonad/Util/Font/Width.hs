module XMonad.Util.Font.Width where

import Graphics.X11.Xft
import Graphics.X11.Xrender

import XMonad(MonadIO, Display, io)
import XMonad.Util.Font

xftWidth :: MonadIO m => Display -> XftFont -> String -> m Int
xftWidth d xftFont s = fmap ((*3) . (`div` 2)) $ io $ xglyphinfo_width <$> xftTextExtents d xftFont s

-- | FIXME: Only works with Xft fonts, currently.
xmfWidth :: MonadIO m => Display -> XMonadFont -> String -> m Int
xmfWidth d fnt s = case fnt of
  (Xft xftFont) -> xftWidth d xftFont s
  
