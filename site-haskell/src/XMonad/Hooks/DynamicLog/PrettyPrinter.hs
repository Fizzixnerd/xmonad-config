module XMonad.Hooks.DynamicLog.PrettyPrinter where

import XMonad hiding (workspaces)
import XMonad.Core hiding (workspaces)

import qualified XMonad.StackSet as S

import XMonad.Util.NamedWindows

import XMonad.Hooks.UrgencyHook

import XMonad.Hooks.DynamicLog.Status.DZen2.Universal
import XMonad.Hooks.DynamicLog.Status.StatusText
import XMonad.Hooks.DynamicLog.Status.StatusText.Dynamic

import qualified Data.Text as T

import Control.Monad.IO.Class
import Control.Monad

import Data.Maybe
import Data.List (intersperse, init)
