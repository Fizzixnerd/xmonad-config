module XMonad.Hooks.DynamicLog.Dzen2.Font where

import XMonad hiding (Font)
import XMonad.Util.Font

newtype FontName = FontName String

class Font f where
  font :: f -> X XMonadFont

instance Font XMonadFont where
  font = return

instance Font FontName where
  font (FontName fn) = initXMF fn
