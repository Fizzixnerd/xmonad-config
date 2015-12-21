{-# LANGUAGE OverloadedStrings #-}

module XMonad.Hooks.DynamicLog.Status.DZen2.Fancy where

import XMonad.Hooks.DynamicLog.Status.System
import XMonad.Hooks.DynamicLog.Status.StatusText
import XMonad.Hooks.DynamicLog.Status.DZen2.Universal

import Control.Monad.IO.Class

colorBattery :: Int -> (String -> StatusText)
colorBattery x
  | (x < 15)  = fg "red"
  | (x < 50)  = fg "yellow"
  | (x < 90)  = fg "green"
  | otherwise = fg "blue"

coloredBattery :: (MonadIO m) => m StatusText 
coloredBattery = do
  batST <- battery
  return $ do 
    batText <- batST
    (colorBattery $ read batText) batText
