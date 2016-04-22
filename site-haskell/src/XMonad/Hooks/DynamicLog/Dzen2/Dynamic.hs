{-# LANGUAGE OverloadedStrings #-}

module XMonad.Hooks.DynamicLog.Dzen2.Dynamic where

import XMonad.Util.Run
import XMonad.Hooks.DynamicLog.Dzen2.StatusText
import XMonad.Hooks.DynamicLog.Dzen2.Format

import Control.Monad.IO.Class
import Data.String

type Process = (String, [String])

dynText :: Monad m => String -> m (StatusText fnt)
dynText = return . mkStatusText

dynProc :: MonadIO m => Process -> m (StatusText fnt)
dynProc (p, args) = mkStatusText <$> runProcessWithInput p args ""
