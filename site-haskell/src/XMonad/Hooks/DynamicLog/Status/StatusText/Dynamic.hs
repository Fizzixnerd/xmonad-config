{-# LANGUAGE OverloadedStrings #-}

module XMonad.Hooks.DynamicLog.Status.StatusText.Dynamic where

-- IMPORTS

import XMonad.Util.Run
import XMonad.Hooks.DynamicLog.Status.StatusText

import Control.Monad.IO.Class
import Data.String

-- TYPES

type Process = (String, [String])

-- FUNCTIONS

dynText :: Monad m => String -> m StatusText
dynText = return . simpleStatusText

dynProc :: MonadIO m => Process -> m StatusText
dynProc (p, args) = (simpleStatusText . fromString) <$> runProcessWithInput p args ""
