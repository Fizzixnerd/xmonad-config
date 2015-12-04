module XMonad.Hooks.DynamicLog.PrettyPrinter where

import XMonad

import XMonad.Hooks.DynamicLog.PrettyPrinter.DynamicDoc

import Control.Monad.IO.Class

date :: MonadIO m => m DynamicDoc
date = fmap DynStr $ runProcess ("date", [])

battery :: MonadIO m => m DynamicDoc
battery = fmap DynStr $ fmap ((takeWhile (/= ',')) . (!! 3) . words) $ runProcess ("acpi", [])
