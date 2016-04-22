{-# LANGUAGE   FlexibleContexts
             , FlexibleInstances #-}

module XMonad.Hooks.DynamicLog.Dzen2.Battery where

import System.Time
import System.Locale

import Data.Monoid

import XMonad (X, MonadIO)
import XMonad.Hooks.DynamicLog.Dzen2.Status
import XMonad.Hooks.DynamicLog.Dzen2.Format
import XMonad.Hooks.DynamicLog.Dzen2.Dynamic
import XMonad.Hooks.DynamicLog.Dzen2.Content
import XMonad.Hooks.DynamicLog.Dzen2.Render

data BatteryInfo = BatteryInfo { charging :: Bool
                               , time :: TimeDiff
                               , percent :: Int
                               , ident :: Int
                               , factoryCapacity :: Maybe Int
                               , lastFullCapacity :: Maybe Int
                               } deriving Show

instance Render BatteryInfo where
  render bi = 
    let (tm, pcnt, chrg) = parseBatteryInfo bi
    in
      pcnt <> chrg <> " " <> tm

instance Content BatteryInfo where
  content = show

newtype FancyBatteryInfo = FancyBatteryInfo BatteryInfo deriving Show

instance Render FancyBatteryInfo where
  render (FancyBatteryInfo fbi@(BatteryInfo { charging = c
                                            , percent = p
                                            , time = t
                                            })) =
    let (tm, pcnt, chrg) = parseBatteryInfo fbi
        color = if p >= 90 then
                  CHex "blue"
                else 
                  if p >= 50 then
                    CHex "green"
                  else
                    if p >= 15 then
                      CHex "yellow"
                    else
                      CHex "red"
    in
      render $ (fg color $ mkStatusT pcnt) <> (mkStatusT chrg) <> (mkStatusT " ") <> (mkStatusT tm)

instance Content FancyBatteryInfo where
  content = show
      
parseBatteryInfo :: BatteryInfo -> (String, String, String)
parseBatteryInfo (BatteryInfo { charging = c
                              , percent = p
                              , time = t
                              }) = 
  (formatTimeDiff defaultTimeLocale "(%H:%M)" t, show p, if c then "+" else "-")

acpi :: MonadIO m => m [BatteryInfo]
acpi = parseAcpi . content <$> dynProc ("acpi", ["-b"])

fancyAcpi :: MonadIO m => m [FancyBatteryInfo]
fancyAcpi = fmap (FancyBatteryInfo <$>) acpi

parseAcpi :: String -> [BatteryInfo]
parseAcpi s = do
  ln <- lines s
  let wrds = words ln
      charging = case (init $ wrds !! 2) of
        "Charging" -> True
        "Discharging" -> False
      time = noTimeDiff { tdHour = read $ take 2 $ wrds !! 4
                        , tdMin = read $ take 2 $ drop 3 $ wrds !! 4
                        , tdSec = read $ take 2 $ drop 6 $ wrds !! 4
                        }
      percent = read $ init $ wrds !! 3
      ident = read $ init $ wrds !! 1
  return BatteryInfo { charging = charging
                     , time = time
                     , percent = percent
                     , ident = ident
                     , factoryCapacity = Nothing
                     , lastFullCapacity = Nothing
                     }
