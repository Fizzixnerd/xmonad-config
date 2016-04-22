 {-# LANGUAGE   FlexibleInstances
              , TypeSynonymInstances #-}

module XMonad.Hooks.DynamicLog.Dzen2.Render where

-- | Return the String representation of a formatable object, with the
-- formatting included.  See StatusText and Status s.
class Render r where
  render :: r -> String

instance Render String where
  render = id
