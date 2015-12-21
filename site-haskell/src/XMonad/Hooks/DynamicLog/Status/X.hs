{-# LANGUAGE OverloadedStrings #-}

module XMonad.Hooks.DynamicLog.Status.X where

import XMonad hiding (workspaces)
import XMonad.Core hiding (workspaces)
import qualified XMonad.StackSet as S
import XMonad.Util.NamedWindows
import XMonad.Hooks.UrgencyHook

import XMonad.Hooks.DynamicLog.Status.StatusText
import XMonad.Hooks.DynamicLog.Status.StatusText.Dynamic

import Data.List
import Data.Maybe
import Data.Monoid
import Data.String

-- Types

data WorkspacePP m = WSPP { wsppCurrent         :: WorkspaceId -> m StatusText
                          , wsppVisible         :: WorkspaceId -> m StatusText
                          , wsppHidden          :: WorkspaceId -> m StatusText
                          , wsppHiddenNoWindows :: WorkspaceId -> m StatusText
                          , wsppUrgent          :: WorkspaceId -> m StatusText
                          , wsppSep             :: m StatusText
                          , wsppSort            :: [S.Workspace WorkspaceId (Layout Window) Window] -> [S.Workspace WorkspaceId (Layout Window) Window]
                          }

data WindowPP m = WPP { wppTitle :: String -> m StatusText }

data LayoutPP m = LPP { lppTitle :: String -> m StatusText }

-- Some simple PPs

defaultWindowPP :: (MonadIO m) => WindowPP m
defaultWindowPP = WPP { wppTitle = dynText }

defaultWorkspacePP :: (Monoid (m String), MonadIO m) => WorkspacePP m
defaultWorkspacePP = WSPP { wsppCurrent         = \x -> dynText $ "[" <> fromString x <> "]"
                          , wsppVisible         = \x -> dynText $ "[" <> fromString x <> "]"
                          , wsppHidden          = dynText . fromString
                          , wsppHiddenNoWindows = const (dynText $ mempty)
                          , wsppUrgent          = \x -> dynText $ "*" <> fromString x <> "*"
                          , wsppSep             = dynText " "
                          , wsppSort            = id
                          }

defaultLayoutPP :: (MonadIO m) => LayoutPP m
defaultLayoutPP = LPP { lppTitle = dynText }

simpleLayoutPP :: (MonadIO m) => LayoutPP m
simpleLayoutPP =  LPP { lppTitle = dynText . last . words }

-- Functions
sepBy :: Monad m
         => m  StatusText  -- ^ separator
         -> m [StatusText] -- ^ fields to output
         -> m [StatusText]
sepBy sep ds = do
  sepST <- sep
  intersperse sepST . reverse . filter (/= simpleStatusText mempty) <$> ds

-- | (Adapted from pprWindowSet from XMonad.Hooks.DynamicLog.)
workspaces :: WorkspacePP X -> X [StatusText]
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
    sepBy (wsppSep wspp) $ sequence $ fmap fmt .  sort' $ fmap S.workspace (S.current winset : S.visible winset) ++ S.hidden winset

defaultWorkspaces :: X [StatusText]
defaultWorkspaces = workspaces defaultWorkspacePP

windowTitle :: WindowPP X -> X StatusText
windowTitle wpp = do 
  winset <- gets windowset
  wt <- maybe (return "") (fmap show . getName) . S.peek $ winset
  (wppTitle wpp) $ fromString $ wt

defaultWindowTitle :: X StatusText
defaultWindowTitle = windowTitle defaultWindowPP

layoutTitle :: LayoutPP X -> X StatusText
layoutTitle lpp = do
  winset <- gets windowset
  (lppTitle lpp) . fromString . description . S.layout . S.workspace . S.current $ winset

defaultLayoutTitle :: X StatusText
defaultLayoutTitle = layoutTitle defaultLayoutPP

simpleLayoutTitle :: X StatusText
simpleLayoutTitle = layoutTitle simpleLayoutPP
