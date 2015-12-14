{-# LANGUAGE OverloadedStrings #-}

module XMonad.Hooks.DynamicLog.Status.System where

import XMonad.Hooks.DynamicLog.Status.StatusText
import XMonad.Hooks.DynamicLog.Status.StatusText.Dynamic

import qualified Data.Text as T

import Data.Monoid
import Control.Monad.IO.Class
import Control.Monad

-- | An (DynamicDoc m) containing the date.
date :: MonadIO m => m StatusText
date = do 
  dateStatusText <- dynProc ("date", [])
  return $ T.init <$> dateStatusText

-- | An (DynamicDoc m) containing the battery percentage, without a percent sign.
battery :: MonadIO m => m StatusText
battery = do
  acpiStatusText <- dynProc ("acpi", [])
  return $ getChargeValue <$> acpiStatusText
  where
    getChargeValue = T.takeWhile (/= '%') . (!! 3) . T.words

-- | An (DynamicDoc m) containing the battery percentage, with a percent sign.
batteryPercentage :: MonadIO m => m StatusText
batteryPercentage = do
  batteryText <- battery
  return $ (<> "%") <$> batteryText 

