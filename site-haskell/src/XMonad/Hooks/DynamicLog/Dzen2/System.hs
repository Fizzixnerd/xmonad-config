{-# LANGUAGE OverloadedStrings #-}

module XMonad.Hooks.DynamicLog.Dzen2.System where

import XMonad.Hooks.DynamicLog.Dzen2.StatusText
import XMonad.Hooks.DynamicLog.Dzen2.Dynamic

import Data.Monoid
import Control.Monad.IO.Class
import Control.Monad

-- | An (m StatusText) containing the date.
date :: MonadIO m => m (StatusText fnt)
date = do 
  dateStatusText <- dynProc ("date", [])
  return $ init <$> dateStatusText

-- | An (m StatusText) containing the battery percentage, without a percent sign.
battery :: MonadIO m => m (StatusText fnt)
battery = do
  acpiStatusText <- dynProc ("acpi", [])
  return $ getChargeValue <$> acpiStatusText
  where
    getChargeValue = takeWhile (/= '%') . (!! 3) . words

-- | An (m StatusText) containing the battery percentage, with a percent sign.
batteryPercentage :: MonadIO m => m (StatusText fnt)
batteryPercentage = do
  batteryText <- battery
  return $ (<> "%") <$> batteryText 

