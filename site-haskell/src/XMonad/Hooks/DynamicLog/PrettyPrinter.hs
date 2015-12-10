
module XMonad.Hooks.DynamicLog.PrettyPrinter where

import XMonad hiding (workspaces)
import XMonad.Core hiding (workspaces)

import qualified XMonad.StackSet as S

import XMonad.Util.NamedWindows

import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.DynamicLog.PrettyPrinter.DynamicDoc

import Text.PrettyPrint hiding (empty)

import Control.Monad.IO.Class

import Data.Monoid
import Data.Maybe
import Data.List (intersperse, init)

data WorkspacePP = WSPP { wsppCurrent         :: WorkspaceId -> DynamicDoc
                        , wsppVisible         :: WorkspaceId -> DynamicDoc
                        , wsppHidden          :: WorkspaceId -> DynamicDoc
                        , wsppHiddenNoWindows :: WorkspaceId -> DynamicDoc
                        , wsppUrgent          :: WorkspaceId -> DynamicDoc
                        , wsppSep             :: DynamicDoc
                        , wsppSort            :: [S.Workspace WorkspaceId (Layout Window) Window] -> [S.Workspace WorkspaceId (Layout Window) Window]
                        }

defaultWorkspacePP :: WorkspacePP
defaultWorkspacePP = WSPP { wsppCurrent         = \x -> dynStr $ "[" ++ x ++ "]"
                          , wsppVisible         = \x -> dynStr $ "[" ++ x ++ "]"
                          , wsppHidden          = dynStr
                          , wsppHiddenNoWindows = const empty
                          , wsppUrgent          = \x -> dynStr $ "*" ++ x ++ "*"
                          , wsppSep             = dynStr " "
                          , wsppSort            = id
                          }

data WindowPP = WPP { wppTitle :: String -> DynamicDoc }

defaultWindowPP :: WindowPP
defaultWindowPP = WPP { wppTitle = dynStr }

data LayoutPP = LPP { lppTitle :: String -> DynamicDoc }

defaultLayoutPP :: LayoutPP
defaultLayoutPP = LPP { lppTitle = dynStr }

liftDynamicDoc :: (String -> String) -> (DynamicDoc -> DynamicDoc)
liftDynamicDoc f = ((text . f . renderStatusBar) .$.)

-- | An (m DynamicDoc) containing the date.
date :: MonadIO m => m DynamicDoc
date = 
  let
    dateDynDoc = dynProc ("date", [])
  in
    return $ init' dateDynDoc
  where 
    init' = liftDynamicDoc init

-- | An (m DynamicDoc) containing the battery percentage, without a percent sign.
battery :: X DynamicDoc
battery = 
  let 
    acpiDynDoc = dynProc ("acpi", [])
  in
    return $ liftDynamicDoc ((takeWhile (/= '%')) . (!! 3) . words) acpiDynDoc

-- | An (m DynamicDoc) containing the battery percentage, with a percent sign.
batteryPercentage :: X DynamicDoc
batteryPercentage = do
  bat <- battery
  return $ bat .+. dynStr "%"

sepBy :: DynamicDoc   -- ^ separator
         -> [DynamicDoc] -- ^ fields to output
         -> DynamicDoc
sepBy sep = magSum . intersperse sep . filter (not . isEmpty)
  where
    isEmpty (Compound ([], _)) = True
    isEmpty _ = False

-- | (Adapted from pprWindowSet from XMonad.Hooks.DynamicLog.)
workspaces :: WorkspacePP -> X DynamicDoc
workspaces wspp = do
  winset  <- gets windowset
  urgents <- readUrgents
  let this     = S.currentTag winset
      visibles = map (S.tag . S.workspace) (S.visible winset)
      sort'    = wsppSort wspp
      fmt w    = printer wspp (S.tag w)
        where printer | any (\x -> maybe False (== S.tag w) (S.findTag x winset)) urgents = wsppUrgent
                      | S.tag w == this                                                   = wsppCurrent
                      | S.tag w `elem` visibles                                           = wsppVisible
                      | isJust (S.stack w)                                                = wsppHidden
                      | otherwise                                                         = wsppHiddenNoWindows
  return $ sepBy (wsppSep wspp) . map fmt . sort' $ map S.workspace (S.current winset : S.visible winset) ++ S.hidden winset

defaultWorkspaces :: X DynamicDoc
defaultWorkspaces = workspaces defaultWorkspacePP

windowTitle :: WindowPP -> X DynamicDoc
windowTitle wpp = do 
  winset <- gets windowset
  wt <- maybe (return "") (fmap show . getName) . S.peek $ winset
  return $ (wppTitle wpp) $ wt

defaultWindowTitle :: X DynamicDoc
defaultWindowTitle = windowTitle defaultWindowPP

layoutTitle :: LayoutPP -> X DynamicDoc
layoutTitle lpp = do
  winset <- gets windowset
  return $ (lppTitle lpp) . description . S.layout . S.workspace . S.current $ winset

defaultLayoutTitle :: X DynamicDoc
defaultLayoutTitle = layoutTitle defaultLayoutPP

myPracticeThing :: X DynamicDoc
myPracticeThing = do
  dwt <- defaultWindowTitle 
  dlt <- defaultLayoutTitle 
  dw  <-defaultWorkspaces
  return $ dwt .+. dlt .+. dw 
