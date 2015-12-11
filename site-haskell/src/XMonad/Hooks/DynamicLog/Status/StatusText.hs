{-# LANGUAGE OverloadedStrings #-}

module XMonad.Hooks.DynamicLog.Status.StatusText where

import qualified Data.Text as T

import Data.Monoid

import Control.Monad.Writer

type Status b a = Writer (b, Dual b) a
type StatusText = Status [T.Text] T.Text

-- | Return the length of the /displayed/ Text (without the control
-- prefix/sufixes).
length :: StatusText -> Int
length st = T.length $ content st

-- | Render the StatusText to a Text suitable for outputting.
render :: StatusText -> T.Text
render st = mconcat [mconcat p, c, mconcat s]
  where
    c = content st
    (p, s) = tags st

-- | Return the human-readable content of the StatusText.
content :: StatusText -> T.Text
content st = fst $ runWriter st

-- | Return the tag wrappers of the StatusText.
tags :: StatusText -> ([T.Text], [T.Text])
tags st = (p, s)
  where 
    (p, Dual s) = snd $ runWriter st

-- | Return a StatusText with the given prefixes, suffixes, and
-- content respectively.
makeStatusText :: [T.Text] -> [T.Text] -> T.Text -> StatusText
makeStatusText p s c = writer (c, (p, Dual s))
