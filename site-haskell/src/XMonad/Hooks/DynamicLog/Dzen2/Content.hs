{-# LANGUAGE   FlexibleInstances
             , TypeSynonymInstances #-}

module XMonad.Hooks.DynamicLog.Dzen2.Content where

-- | Return the String content of a formatable object, without
-- formatting.  See StatusText and Status s.
class Content c where
  content :: c -> String

instance Content String where
  content = id

