{-# LANGUAGE OverloadedStrings #-}

module XMonad.Hooks.DynamicLog.Status.StatusText.Dynamic where

-- IMPORTS

import XMonad.Util.Run
import XMonad.Hooks.DynamicLog.Status.StatusText

import qualified Data.Text as T

import Control.Monad.IO.Class
import Data.String

-- TYPES

type Process = (T.Text, [T.Text])

-- FUNCTIONS

dynText :: Monad m => T.Text -> m StatusText
dynText = return . simpleStatusText

dynProc :: MonadIO m => Process -> m StatusText
dynProc (p, args) = (simpleStatusText . fromString) <$> runProcessWithInput (T.unpack p) (fmap T.unpack args) ""
