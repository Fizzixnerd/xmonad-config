{-# LANGUAGE OverloadedStrings #-}

module XMonad.Hooks.DynamicLog.Status.StatusText where

import Data.Monoid

import Control.Monad.Writer

type Status b a = Writer (b, Dual b) a
type StatusText = Status [String] String

-- | Return the length of the /displayed/ String (without the control
-- prefix/sufixes).
length :: StatusText -> Int
length st = Prelude.length $ content st

-- | Render the StatusText to a Text suitable for outputting.
render :: StatusText -> String
render st = mconcat [mconcat p, c, mconcat s]
  where
    c = content st
    (p, s) = tags st

-- | Return the human-readable content of the StatusText.
content :: StatusText -> String
content st = fst $ runWriter st

-- | Return the tag wrappers of the StatusText.
tags :: StatusText -> ([String], [String])
tags st = (p, s)
  where 
    (p, Dual s) = snd $ runWriter st

-- | Return a StatusText with the given prefixes, suffixes, and
-- content respectively.
makeStatusText :: [String] -> [String] -> String -> StatusText
makeStatusText p s c = writer (c, (p, Dual s))

simpleStatusText :: String -> StatusText
simpleStatusText = return

dynST :: Monad m => String -> m StatusText
dynST = return . simpleStatusText
