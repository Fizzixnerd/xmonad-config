module XMonad.Hooks.DynamicLog.PrettyPrinter where

import XMonad hiding (workspaces)
import XMonad.Core hiding (workspaces)

import qualified XMonad.StackSet as S

import XMonad.Util.NamedWindows

import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.DynamicLog.PrettyPrinter.DynamicDoc

import Control.Monad.IO.Class

import Data.Monoid
import Data.Maybe
import Data.List (intersperse)

data WorkspacePP = WSPP { wsppCurrent         :: WorkspaceId -> DynamicDoc
                        , wsppVisible         :: WorkspaceId -> DynamicDoc
                        , wsppHidden          :: WorkspaceId -> DynamicDoc
                        , wsppHiddenNoWindows :: WorkspaceId -> DynamicDoc
                        , wsppUrgent          :: WorkspaceId -> DynamicDoc
                        , wsppSep             :: DynamicDoc
                        , wsppSort            :: [S.Workspace WorkspaceId (Layout Window) Window] -> [S.Workspace WorkspaceId (Layout Window) Window]
                        }

defaultWorkspacePP :: WorkspacePP
defaultWorkspacePP = WSPP { wsppCurrent         = \x -> DynStr $ "[" ++ x ++ "]"
                          , wsppVisible         = \x -> DynStr $ "[" ++ x ++ "]"
                          , wsppHidden          = DynStr
                          , wsppHiddenNoWindows = const mempty
                          , wsppUrgent          = \x -> DynStr $ "*" ++ x ++ "*"
                          , wsppSep             = DynStr " "
                          , wsppSort            = id
                          }

data WindowPP = WPP { wppTitle :: String -> DynamicDoc }

defaultWindowPP :: WindowPP
defaultWindowPP = WPP { wppTitle = DynStr }

data LayoutPP = LPP { lppTitle :: String -> DynamicDoc }

defaultLayoutPP :: LayoutPP
defaultLayoutPP = LPP { lppTitle = DynStr }

-- | An (m DynStr) containing the date.
date :: MonadIO m => m DynamicDoc
date = do
  theDate <- runProcess ("date", [])
  return $ DynStr theDate

-- | An (m DynStr) containing the battery percentage, without a percent sign.
battery :: MonadIO m => m DynamicDoc
battery = do
  acpiStr <- runProcess ("acpi", [])
  return $ DynStr . (takeWhile (/= '%')) . (!! 3) . words $ acpiStr

-- | An (m DynStr) containing the battery percentage, with a percent sign.
batteryPercentage :: MonadIO m => m DynamicDoc
batteryPercentage = do
  bat <- battery
  return $ bat <> DynStr "%"

sepBy :: DynamicDoc   -- ^ separator
         -> [DynamicDoc] -- ^ fields to output
         -> DynamicDoc
sepBy sep = mconcat . intersperse sep . filter (not . (== mempty))


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
myPracticeThing = defaultWindowTitle <> defaultLayoutTitle <> defaultWorkspaces
