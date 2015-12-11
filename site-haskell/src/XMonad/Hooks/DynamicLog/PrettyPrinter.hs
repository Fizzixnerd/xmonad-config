module XMonad.Hooks.DynamicLog.PrettyPrinter where

import XMonad hiding (workspaces)
import XMonad.Core hiding (workspaces)

import qualified XMonad.StackSet as S

import XMonad.Util.NamedWindows

import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.DynamicLog.PrettyPrinter.DynamicDoc
import XMonad.Hooks.DynamicLog.PrettyPrinter.DZen2

import Text.PrettyPrint

import Control.Monad.IO.Class
import Control.Monad

import Data.Maybe
import Data.List (intersperse, init)

data WorkspacePP m = WSPP { wsppCurrent         :: WorkspaceId -> DynamicDoc m
                          , wsppVisible         :: WorkspaceId -> DynamicDoc m
                          , wsppHidden          :: WorkspaceId -> DynamicDoc m
                          , wsppHiddenNoWindows :: WorkspaceId -> DynamicDoc m
                          , wsppUrgent          :: WorkspaceId -> DynamicDoc m
                          , wsppSep             :: Doc
                          , wsppSort            :: [S.Workspace WorkspaceId (Layout Window) Window] -> [S.Workspace WorkspaceId (Layout Window) Window]
                          }

defaultWorkspacePP :: (Monoid (m Doc), MonadIO m) => WorkspacePP m
defaultWorkspacePP = WSPP { wsppCurrent         = \x -> dynStr $ "[" ++ x ++ "]"
                          , wsppVisible         = \x -> dynStr $ "[" ++ x ++ "]"
                          , wsppHidden          = dynStr
                          , wsppHiddenNoWindows = const mempty
                          , wsppUrgent          = \x -> dynStr $ "*" ++ x ++ "*"
                          , wsppSep             = text " "
                          , wsppSort            = id
                          }

data WindowPP m = WPP { wppTitle :: String -> DynamicDoc m }

defaultWindowPP :: (MonadIO m) => WindowPP m
defaultWindowPP = WPP { wppTitle = dynStr }

data LayoutPP m = LPP { lppTitle :: String -> DynamicDoc m }

defaultLayoutPP :: (MonadIO m) => LayoutPP m
defaultLayoutPP = LPP { lppTitle = dynStr }

simpleLayoutPP :: (MonadIO m) => LayoutPP m
simpleLayoutPP =  LPP { lppTitle = dynStr . last . words }

-- | An (DynamicDoc m) containing the date.
date :: MonadIO m => DynamicDoc m
date = init' dateDynDoc
  where init' = fromStrFOL init
        dateDynDoc = dynProc ("date", [])

-- | An (DynamicDoc m) containing the battery percentage, without a percent sign.
battery :: MonadIO m => DynamicDoc m
battery = fromStrFOL ((takeWhile (/= '%')) . (!! 3) . words) acpiDynDoc
  where acpiDynDoc = dynProc ("acpi", [])

-- | An (DynamicDoc m) containing the battery percentage, with a percent sign.
batteryPercentage :: (MonadIO m, MonadPlus m) => DynamicDoc m
batteryPercentage = do
  bat <- battery
  return $ bat <> (text "%")

sepBy :: Monad m
         =>  Doc           -- ^ separator
         -> [DynamicDoc m] -- ^ fields to output
         ->  DynamicDoc m
sepBy sep ds = mconcat <$> intersperse sep . reverse . (foldl (\acc x -> if (x /= empty) then
                                                                           x : acc
                                                                         else
                                                                           acc)
                                                        []) <$> sequence ds

-- msum . intersperse sep . fmap (\x -> if isEmpty x then

-- | (Adapted from pprWindowSet from XMonad.Hooks.DynamicLog.)
workspaces :: WorkspacePP X -> DynamicDoc X
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
                      | otherwise                                                         = wsppHiddenNoWindows in
    sepBy (wsppSep wspp) . map fmt . sort' $ map S.workspace (S.current winset : S.visible winset) ++ S.hidden winset

defaultWorkspaces :: DynamicDoc X
defaultWorkspaces = workspaces defaultWorkspacePP

windowTitle :: WindowPP X -> DynamicDoc X
windowTitle wpp = do 
  winset <- gets windowset
  wt <- maybe (return "") (fmap show . getName) . S.peek $ winset
  (wppTitle wpp) $ wt

defaultWindowTitle :: DynamicDoc X
defaultWindowTitle = windowTitle defaultWindowPP

layoutTitle :: LayoutPP X -> DynamicDoc X
layoutTitle lpp = do
  winset <- gets windowset
  (lppTitle lpp) . description . S.layout . S.workspace . S.current $ winset

defaultLayoutTitle :: DynamicDoc X
defaultLayoutTitle = layoutTitle defaultLayoutPP

simpleLayoutTitle :: DynamicDoc X
simpleLayoutTitle = layoutTitle simpleLayoutPP

