{-# LANGUAGE OverloadedStrings #-}

module XMonad.Hooks.DynamicLog.Dzen2.X where

import XMonad hiding (workspaces)
import XMonad.Core hiding (workspaces)
import qualified XMonad.StackSet as S
import XMonad.Util.NamedWindows
import XMonad.Hooks.UrgencyHook

import XMonad.Hooks.DynamicLog.Dzen2.Format
import XMonad.Hooks.DynamicLog.Dzen2.StatusText
import XMonad.Hooks.DynamicLog.Dzen2.Dynamic

import Data.List
import Data.Maybe
import Data.Monoid
import Data.String

 -- Types

data WorkspacePP m fnt = WSPP { wsppCurrent         :: WorkspaceId -> m (StatusText fnt)
                              , wsppVisible         :: WorkspaceId -> m (StatusText fnt)
                              , wsppHidden          :: WorkspaceId -> m (StatusText fnt)
                              , wsppHiddenNoWindows :: WorkspaceId -> m (StatusText fnt)
                              , wsppUrgent          :: WorkspaceId -> m (StatusText fnt)
                              , wsppSep             :: m (StatusText fnt)
                              , wsppSort            :: [S.Workspace WorkspaceId (Layout Window) Window] -> [S.Workspace WorkspaceId (Layout Window) Window]
                              }

data WindowPP m fnt = WPP { wppTitle :: String -> m (StatusText fnt) }

data LayoutPP m fnt = LPP { lppTitle :: String -> m (StatusText fnt) }

 -- Some simple PPs

defaultWindowPP :: (MonadIO m) => WindowPP m fnt
defaultWindowPP = WPP { wppTitle = dynText }

defaultWorkspacePP :: (Monoid (m String), MonadIO m) => WorkspacePP m fnt
defaultWorkspacePP = WSPP { wsppCurrent         = \x -> dynText $ "[" <> x <> "]"
                          , wsppVisible         = \x -> dynText $ "[" <> x <> "]"
                          , wsppHidden          = dynText
                          , wsppHiddenNoWindows = const (dynText $ mempty)
                          , wsppUrgent          = \x -> dynText $ "*" <> x <> "*"
                          , wsppSep             = dynText " "
                          , wsppSort            = id
                          }

defaultLayoutPP :: (MonadIO m) => LayoutPP m fnt
defaultLayoutPP = LPP { lppTitle = dynText }

simpleLayoutPP :: (MonadIO m) => LayoutPP m fnt
simpleLayoutPP =  LPP { lppTitle = dynText . last . words }

-- Functions
sepBy :: Monad m
         => m (StatusText fnt) -- ^ separator
         -> m [StatusText fnt] -- ^ fields to output
         -> m [StatusText fnt]
sepBy sep ds = do
  sepST <- sep
  intersperse sepST . reverse . filter (/= mkStatusText mempty) <$> ds

-- | (Adapted from pprWindowSet from XMonad.Hooks.DynamicLog.)
workspaces :: WorkspacePP X fnt -> X [StatusText fnt]
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
    sepBy (wsppSep wspp) $ sequence $ fmap fmt . sort' $ fmap S.workspace (S.current winset : S.visible winset) ++ S.hidden winset

-- | TODO: Implement
--   FIXME: Implement!
defaultWorkspaces :: X [StatusText fnt]
defaultWorkspaces = (workspaces defaultWorkspacePP) >>= (\xs -> undefined)

windowTitle :: WindowPP X fnt -> X (StatusText fnt)
windowTitle wpp = do 
  winset <- gets windowset
  wt <- maybe (return "") (fmap show . getName) . S.peek $ winset
  (wppTitle wpp) $ fromString $ wt

defaultWindowTitle :: X (StatusText fnt)
defaultWindowTitle = windowTitle defaultWindowPP

layoutTitle :: LayoutPP X fnt -> X (StatusText fnt)
layoutTitle lpp = do
  winset <- gets windowset
  (lppTitle lpp) . fromString . description . S.layout . S.workspace . S.current $ winset

defaultLayoutTitle :: X (StatusText fnt)
defaultLayoutTitle = layoutTitle defaultLayoutPP

simpleLayoutTitle :: X (StatusText fnt)
simpleLayoutTitle = layoutTitle simpleLayoutPP
