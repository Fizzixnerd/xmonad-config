{-# LANGUAGE   MultiParamTypeClasses
             , FlexibleInstances
             , FlexibleContexts #-}

module XMonad.Hooks.DynamicLog.Dzen2.Width where

import XMonad hiding (Font, Status)
import XMonad.Util.Font
import XMonad.Util.Font.Width
import XMonad.Hooks.DynamicLog.Dzen2.Content
import XMonad.Hooks.DynamicLog.Dzen2.Font
import XMonad.Hooks.DynamicLog.Dzen2.StatusText
import XMonad.Hooks.DynamicLog.Dzen2.Status

type PixelWidth = Int

class Monad m => Width w m where
  width :: w -> m PixelWidth

-- | Return the width in pixels of the StatusText.  FIXME: This only
-- works on xftFonts
instance Font (StatusText fnt) => Width (StatusText fnt) X where
  width x = do
    fnt <- font x
    width (unpack x, fnt)

instance Width (Status String, XMonadFont) X where
  width (s, xmf) = withDisplay (\disp -> xmfWidth disp xmf $ content s)

-- import XMonad hiding (Status)
-- import XMonad.Util.Font
-- import XMonad.Util.Font.Width
-- import XMonad.Hooks.DynamicLog.Dzen2.Font
-- import XMonad.Hooks.DynamicLog.Dzen2.StatusText

-- type PixelWidth = Int

-- -- | The width of something in _pixels_
-- class Monad m => Width s m where
--   width :: s -> m PixelWidth
--   width = const $ return 0

-- instance {-# OVERLAPS #-} Monad m => Width Static m where
--   width (SI _ (Dimension w) _) = return w
--   width (SR (Dimension w) _) = return w
--   width (SRO (Dimension w) _) = return w
--   width (SC (Dimension r)) = return $ 2 * r
--   width (SCO (Dimension r)) = return $ 2 * r
--   width s = return 0

-- instance {-# OVERLAPS #-} Width s m => Width (Status s) m where
--   width (SAtom x) = width x
--   width (SList xs) = fmap sum $ sequence $ fmap width xs
--   width (SP _ x) = width x
--   width (SPA _ x) = width x
--   width (SFG _ x) = width x
--   width (SBG _ x) = width x
--   width (SCA _ _ x) = width x
--   width (SStatic s) = width s

-- instance {-# OVERLAPS #-} Width StatusText X where
--   width (StatusText (fnt, s)) = do
--     xftFont <- font fnt
--     withDisplay (\disp -> xftWidth disp xftFont s)
