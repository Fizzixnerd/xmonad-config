{-# LANGUAGE   FlexibleInstances
             , FlexibleContexts #-}

module XMonad.Hooks.DynamicLog.Dzen2.StatusText where

import XMonad hiding (Status, Position, Color, Dimension, Font)
import XMonad.Hooks.DynamicLog.Dzen2.Font
import XMonad.Hooks.DynamicLog.Dzen2.Render
import XMonad.Hooks.DynamicLog.Dzen2.Content
import XMonad.Hooks.DynamicLog.Dzen2.Status
import XMonad.Util.Font
import XMonad.Util.Font.Width

import qualified Data.List as L

newtype StatusT fnt s = StatusT (Status s) deriving Eq
type StatusText fnt = StatusT fnt String

instance Content s => Content (StatusT fnt s) where
  content (StatusT (SAtom x)) = content x
  content (StatusT (SList xs)) = mconcat $ content <$> xs
  content (StatusT (SP _ x)) = content x
  content (StatusT (SPA _ x)) = content x
  content (StatusT (SFG _ x)) = content x
  content (StatusT (SBG _ x)) = content x
  content (StatusT (SCA _ _ x)) = content x
  content (StatusT (SStatic s)) = ""

instance Render s => Render (StatusT fnt s) where
  render (StatusT x) = render x

instance Functor (StatusT fnt) where
  fmap f (StatusT s) = StatusT $ fmap f s

instance Applicative (StatusT fnt) where
  pure = StatusT . SAtom
  (StatusT f) <*> (StatusT s) = StatusT $ f <*> s

instance Monad (StatusT fnt) where
  return = pure
  (StatusT x) >>= f = StatusT $ x >>= (\(StatusT x) -> x) . f

instance Monoid (StatusT fnt s) where
  mempty = StatusT $ mempty
  (StatusT x) `mappend` (StatusT y) = StatusT (x `mappend` y)
  
-- | Return the logical length of the StatusText, in number of
-- characters.  That is, the length of its content.
length :: StatusText fnt -> Int
length = L.length . content

onto :: (Status s -> Status t) -> StatusT fnt s -> StatusT fnt t
onto f (StatusT s) = StatusT $ f s

pack :: Status s -> StatusT fnt s
pack s = StatusT s

unpack :: StatusT fnt s -> Status s
unpack (StatusT s) = s
