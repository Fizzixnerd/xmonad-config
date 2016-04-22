{-# LANGUAGE FlexibleInstances #-}

module XMonad.Hooks.DynamicLog.Dzen2.Status where

import XMonad.Hooks.DynamicLog.Dzen2.Render
import XMonad.Hooks.DynamicLog.Dzen2.Content

import Data.Monoid
import Data.Functor
import qualified Data.List as L
import Control.Applicative

data Position = PX Int | PY Int | PXY Int Int deriving Eq
instance Render Position where
   render (PX x)
     | x > 0 = "+" <> show x
     | otherwise = show x
   render (PY y)
     | y > 0 = ";" <> "+" <> show y
     | otherwise = ";" <> show y
   render (PXY x y) = (render $ PX x) <> (render $ PY y)

data RelativePosition = RPLeft | RPRight | RPCenter
                      | RPTop | RPBottom
                      | RPLockX | RPUnlockX 
                      | RPPosition Position deriving Eq
instance Render RelativePosition where
  render RPLeft = "_LEFT"
  render RPRight = "_RIGHT"
  render RPCenter = "_CENTER"
  render RPTop = "_TOP"
  render RPBottom = "_BOTTOM"
  render RPLockX = "_LOCK_X"
  render RPUnlockX = "_UNLOCK_X"
  render (RPPosition p) = render p

newtype AbsolutePosition = APPosition Position deriving Eq
instance Render AbsolutePosition where render (APPosition p) = render p

newtype Color = CHex String deriving Eq
instance Render Color where render (CHex s) = s

newtype Dimension = Dimension Int deriving Eq
instance Render Dimension where render (Dimension x) = if x > 0 then show x else "0"

data State = On | Off | Toggle deriving Eq

data MouseButton = MBLeft | MBRight | MBMiddle deriving Eq
instance Render MouseButton where
  render MBLeft = "1"
  render MBRight = "2"
  render MBMiddle = "3"

newtype ShellCommand = ShellCommand String deriving Eq
instance Render ShellCommand where render (ShellCommand s) = s

data Static = SEmpty -- Monoidal Identity
              -- Drawables (icons [in XBM or XPM], rectangles [filled
              -- and outlined], and circles [filled and outlined]).
              -- Note that the width and height of the icon must be
              -- given.
            | SI  FilePath Dimension Dimension
            | SR  Dimension Dimension
            | SRO Dimension Dimension
            | SC  Dimension
            | SCO Dimension
              -- Dzen2 'actions'.
            | SCollapse State
            | SStick    State
            | SHide     State
            | SRaise  
            | SLower 
            | SScrollHome
            | SScrollEnd
            | SExit deriving Eq
instance Render Static where
  render SEmpty = mempty
  render (SI fp _ _) = "^i(" <> render fp <> ")"
  render (SR w h) = "^r(" <> render w <> "x" <> render h <> ")"
  render (SRO w h) = "^ro(" <> render w <> "x" <> render h <> ")"
  render (SC r) = "^c(" <> render r <> ")"
  render (SCO r) = "^co(" <> render r <> ")"
  render (SCollapse st) 
    | st == On = "^collapse()"
    | st == Off = "^uncollapse()"
    | st == Toggle = "^togglecollapse()"
  render (SStick st) 
    | st == On = "^stick()"
    | st == Off = "^unstick()"
    | st == Toggle = "^togglestick()"
  render (SHide st)
    | st == On = "^hide()"
    | st == Off = "^unhide()"
    | st == Toggle = "^togglehide()"
  render SRaise = "^raise()"
  render SLower = "^lower()"
  render SScrollHome = "^scrollhome()"
  render SScrollEnd = "^scrollend()"
  render SExit = "^exit()"

data Status s = SAtom s
              | SList [Status s]
                -- Positions (relative and absolute).
              | SP  RelativePosition (Status s)
              | SPA AbsolutePosition (Status s)
                -- Foreground and background colors.
              | SFG Color            (Status s)
              | SBG Color            (Status s)
              | SCA MouseButton ShellCommand (Status s)
                -- Monoidal identity
              | SStatic Static deriving Eq

instance Content s => Content (Status s) where
  content (SAtom x) = content x
  content (SList xs) = mconcat $ content <$> xs
  content (SP _ x) = content x
  content (SPA _ x) = content x
  content (SFG _ x) = content x
  content (SBG _ x) = content x
  content (SCA _ _ x) = content x
  content (SStatic s) = ""

instance Render s => Render (Status s) where
  render (SAtom x)  = render x
  render (SList xs) = mconcat $ fmap render xs
  render (SP rp x)  = "^p(" <> render rp <> ")" <> render x
  render (SPA ap x) = "^pa(" <> render ap <> ")" <> render x
  render (SFG c x)  = "^fg(" <> render c <> ")" <> render x <> render "^fg()"
  render (SBG c x)  = "^bg(" <> render c <> ")" <> render x <> render "^bg()"
  render (SCA mb sc x) = "^ca(" <> render mb <> ", " <> render sc <> ")" <> render x <> "^ca()"
  render (SStatic s) = render s

instance Functor Status where
  fmap f (SAtom  x)  = SAtom  $ f x
  fmap f (SList  xs) = SList  $ fmap (fmap f) xs
  fmap f (SP  rp x)  = SP rp  $ fmap f x
  fmap f (SPA ap x)  = SPA ap $ fmap f x
  fmap f (SFG c  x)  = SFG c  $ fmap f x
  fmap f (SBG c  x)  = SBG c  $ fmap f x
  fmap f (SCA mb sc x) = SCA mb sc $ fmap f x
  fmap f (SStatic s) = SStatic s

instance Applicative Status where
  pure x = SAtom x
  (SAtom  f)  <*> x = fmap f x
  (SList  fs) <*> x = SList  $ fmap (<*> x) fs
  (SP  rp f)  <*> x = SP  rp $ f <*> x
  (SPA ap f)  <*> x = SPA ap $ f <*> x
  (SFG c  f)  <*> x = SFG c  $ f <*> x
  (SBG c  f)  <*> x = SBG c  $ f <*> x
  (SCA mb sc f) <*> x = SCA mb sc $ f <*> x
  (SStatic s) <*> x = SStatic s

instance Monad Status where
  (SAtom x)  >>= f = f x
  (SList xs) >>= f = SList $ fmap (>>= f) xs
  (SP rp x)  >>= f = SP rp $ x >>= f
  (SPA ap x) >>= f = SPA ap $ x >>= f
  (SFG c x) >>= f = SFG c $ x >>= f
  (SBG c x) >>= f = SBG c $ x >>= f
  (SCA mb sc x) >>= f = SCA mb sc $ x >>= f
  (SStatic s) >>= f = SStatic s

instance Monoid (Status s) where
  mempty = SStatic SEmpty
  mappend (SList xs) (SList ys) = SList $ xs `mappend` ys
  mappend (SList xs) y = SList $ xs `mappend` (pure y)
  mappend x (SList ys) = SList $ (pure x) `mappend` ys
  mappend x (SStatic SEmpty) = x
  mappend (SStatic SEmpty) y = y
  mappend x y = SList [x, y]
