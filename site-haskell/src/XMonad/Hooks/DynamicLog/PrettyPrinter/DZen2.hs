{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module XMonad.Hooks.DynamicLog.PrettyPrinter.DZen2 where

import XMonad hiding (Color, Position, Status, Hide)

import XMonad.Util.Run

import XMonad.Hooks.DynamicLog.PrettyPrinter.DynamicDoc
import XMonad.Hooks.ManageDocks hiding (R)

import XMonad.Layout.LayoutModifier

import Text.PrettyPrint hiding (sep, (<+>), empty)

import Data.List
import qualified Data.Map as M
import Control.Monad (liftM2)


data Position = LEFT | RIGHT | CENTER 
              | VALUE Int 
              | HERE 
              | RESET_Y 
              | LOCK_X | UNLOCK_X 
              | TOP | BOTTOM
data MouseButton = LeftMB | MiddleMB | RightMB
data Command  = P | PA 
              | FG | BG 
              | I | R | RO | C | CO 
              | CA
              | ToggleCollapse | Collapse | Uncollapse
              | ToggleStick | Stick | Unstick
              | ToggleHide | Hide | Unhide
              | Raise | Lower
              | ScrollHome | ScrollEnd
              | Exit
data Argument = PosArg Position 
              | ColorArg Color 
              | FilePathArg FilePath 
              | DimArg Int 
              | MouseButtonArg MouseButton 
              | ShellCommandArg ShellCommand

type ShellCommand = String
type Color = String
type Status = (Doc, Doc, Doc)

instance Show Position where
  show LEFT = "_LEFT"
  show RIGHT = "_RIGHT"
  show CENTER = "_CENTER"
  show (VALUE v) = show v
  show HERE = ""
  show RESET_Y = ""
  show LOCK_X = "_LOCK_X"
  show UNLOCK_X = "_UNLOCK_X"
  show TOP = "_TOP"
  show BOTTOM = "_BOTTOM"

dynamicStatusBar :: LayoutClass l Window
                    => String -- ^ The command line to launch the status bar.
                    -> DynamicDoc X -- ^ The DynamicDoc
                    -> (XConfig Layout -> (KeyMask, KeySym))
                              -- ^ The desired key binding to toggle bar visibility.
                    -> XConfig l -- ^ The base config.
                    -> X (XConfig (ModifiedLayout AvoidStruts l))
dynamicStatusBar cmd doc k conf = do
  h <- spawnPipe cmd
  return $ conf 
    { layoutHook = avoidStruts $ layoutHook conf 
    , logHook    = do
        logHook conf
        hPrintDynamicDoc h doc
    , manageHook = manageHook conf <+> manageDocks 
    , keys       = liftM2 M.union keys' (keys conf)
    }
      where
         keys' = (`M.singleton` sendMessage ToggleStruts) . k

-- -- | Modifies the given base configuration to launch the given status bar,
-- -- send status information to that bar, and allocate space on the screen edges
-- -- for the bar.
-- statusBar :: LayoutClass l Window
--           => String    -- ^ the command line to launch the status bar
--           -> PP        -- ^ the pretty printing options
--           -> (XConfig Layout -> (KeyMask, KeySym))
--                        -- ^ the desired key binding to toggle bar visibility
--           -> XConfig l -- ^ the base config
--           -> IO (XConfig (ModifiedLayout AvoidStruts l))
-- statusBar cmd pp k conf = do
--     h <- spawnPipe cmd
--     return $ conf
--         { layoutHook = avoidStruts (layoutHook conf)
--         , logHook = do
--               logHook conf
--               dynamicLogWithPP pp { ppOutput = hPutStrLn h }
--         , manageHook = manageHook conf <+> manageDocks
--         , keys       = liftM2 M.union keys' (keys conf)
--         }
--  where
--     keys' = (`M.singleton` sendMessage ToggleStruts) . k

-- -- | The same as 'dynamicLogWithPP', except it simply returns the status
-- --   as a formatted string without actually printing it to stdout, to
-- --   allow for further processing, or use in some application other than
-- --   a status bar.
-- dynamicLogString :: PP -> X String
-- dynamicLogString pp = do

--      winset <- gets windowset
--      urgents <- readUrgents
--      sort' <- ppSort pp

--      -- layout description
--      let ld = description . S.layout . S.workspace . S.current $ winset

--      -- workspace list
--      let ws = pprWindowSet sort' urgents pp winset

--      -- window title
--      wt <- maybe (return "") (fmap show . getName) . S.peek $ winset

--      -- run extra loggers, ignoring any that generate errors.
--      extras <- mapM (flip catchX (return Nothing)) $ ppExtras pp

--      return $ encodeString . sepBy (ppSep pp) . ppOrder pp $
--                          [ ws
--                          , ppLayout pp ld
--                          , ppTitle  pp wt
--                          ]
--                          ++ catMaybes extras

-- -- | Format the workspace information, given a workspace sorting function,
-- --   a list of urgent windows, a pretty-printer format, and the current
-- --   WindowSet.
-- 
-- pprWindowSet sort' urgents pp s = sepBy (ppWsSep pp) . map fmt . sort' $
--                                   map S.workspace (S.current s : S.visible s) ++ S.hidden s
--     where this     = S.currentTag s
--           visibles = map (S.tag . S.workspace) (S.visible s)

--           fmt w = printer pp (S.tag w)
--            where printer | any (\x -> maybe False (== S.tag w) (S.findTag x s)) urgents  = ppUrgent
--                          | S.tag w == this                                               = ppCurrent
--                          | S.tag w `elem` visibles                                       = ppVisible
--                          | isJust (S.stack w)                                            = ppHidden
--                          | otherwise                                                     = ppHiddenNoWindows

escape :: Doc
escape = zeroWidthText "^"

pDoc :: Doc
pDoc = escape <> zeroWidthText "p"

paDoc :: Doc
paDoc = escape <> zeroWidthText "pa"

fgDoc :: Doc
fgDoc = escape <> zeroWidthText "fg"

bgDoc :: Doc
bgDoc = escape <> zeroWidthText "bg"

iDoc :: Doc
iDoc = escape <> zeroWidthText "i"

rDoc :: Doc
rDoc = escape <> zeroWidthText "r"

roDoc :: Doc
roDoc = escape <> zeroWidthText "ro"

cDoc :: Doc
cDoc = escape <> zeroWidthText "c"

coDoc :: Doc
coDoc = escape <> zeroWidthText "co"

caDoc :: Doc
caDoc = escape <> zeroWidthText "ca"

toggleCollapseDoc :: Doc
toggleCollapseDoc = escape <> zeroWidthText "togglecollapse"

collapseDoc :: Doc
collapseDoc = escape <> zeroWidthText "collapse"

uncollapseDoc :: Doc
uncollapseDoc = escape <> zeroWidthText "uncollapse"

toggleStickDoc :: Doc
toggleStickDoc = escape <> zeroWidthText "togglestick"

stickDoc :: Doc
stickDoc = escape <> zeroWidthText "stick"

unstickDoc :: Doc
unstickDoc = escape <> zeroWidthText "unstick"

toggleHideDoc :: Doc
toggleHideDoc = escape <> zeroWidthText "togglehide"

hideDoc :: Doc
hideDoc = escape <> zeroWidthText "hide"

unhideDoc :: Doc
unhideDoc = escape <> zeroWidthText "unhide"

raiseDoc :: Doc
raiseDoc = escape <> zeroWidthText "raise"

lowerDoc :: Doc
lowerDoc = escape <> zeroWidthText "lower"

scrollHomeDoc :: Doc
scrollHomeDoc = escape <> zeroWidthText "scrollhome"

scrollEndDoc :: Doc
scrollEndDoc = escape <> zeroWidthText "scrollend"

exitDoc :: Doc
exitDoc = escape <> zeroWidthText "exit"

ppCommand :: Command -> Doc
ppCommand P = pDoc
ppCommand PA = paDoc
ppCommand FG = fgDoc
ppCommand BG = bgDoc
ppCommand I = iDoc
ppCommand R = rDoc
ppCommand RO = roDoc
ppCommand C = cDoc
ppCommand CO = coDoc
ppCommand CA = caDoc
ppCommand ToggleCollapse = toggleCollapseDoc
ppCommand Collapse = collapseDoc
ppCommand Uncollapse = uncollapseDoc
ppCommand ToggleStick = toggleStickDoc
ppCommand Stick = stickDoc
ppCommand Unstick = unstickDoc
ppCommand ToggleHide = toggleHideDoc
ppCommand Hide = hideDoc
ppCommand Unhide = unhideDoc
ppCommand Raise = raiseDoc
ppCommand Lower = lowerDoc
ppCommand ScrollHome = scrollHomeDoc
ppCommand ScrollEnd = scrollEndDoc
ppCommand Exit = exitDoc

ppPosition :: Position -> Doc
ppPosition HERE = mempty
ppPosition RESET_Y = mempty
ppPosition p = zeroWidthText $ show p

ppMouseButton :: MouseButton -> Doc
ppMouseButton LeftMB = zeroWidthText "1"
ppMouseButton RightMB = zeroWidthText "2"
ppMouseButton MiddleMB = zeroWidthText "3"

ppShellCommand :: ShellCommand -> Doc
ppShellCommand sc = zeroWidthText sc

ppArgument :: Argument -> Doc
ppArgument (PosArg p) = ppPosition p
ppArgument (ColorArg c) = zeroWidthText c
ppArgument (FilePathArg fp) = zeroWidthText fp
ppArgument (DimArg i) = zeroWidthText $ show i
ppArgument (MouseButtonArg mb) = ppMouseButton mb
ppArgument (ShellCommandArg sc) = ppShellCommand sc

delimitedCommand :: Doc -> Doc -> [Doc] -> Doc -> Doc -> Doc
delimitedCommand com sep args enclosed closer =
  (simpleCommand com sep args) <> enclosed <> closer

dzen2DelimitedCommand :: Command -> Doc -> [Argument] -> Doc -> Doc
dzen2DelimitedCommand com sep args enclosed = 
  delimitedCommand (ppCommand com) sep (map ppArgument args) enclosed (simpleCommand (ppCommand com) sep [])

simpleCommand :: Doc -> Doc -> [Doc] -> Doc
simpleCommand com sep args = com <> (parens $ hcat $ intersperse sep args)

dzen2SimpleCommand :: Command -> Doc -> [Argument] -> Doc
dzen2SimpleCommand com sep args = simpleCommand (ppCommand com) sep $ map ppArgument args

pSep :: Doc
pSep = zeroWidthText ";"

dimensionSep :: Doc
dimensionSep = zeroWidthText "x"

clickableSep :: Doc
clickableSep = zeroWidthText ","

p :: Position -> Position -> Doc
p xpos ypos = dzen2SimpleCommand P pSep $ map PosArg [xpos, ypos]

pa :: Position -> Position -> Doc
pa xpos ypos = dzen2SimpleCommand PA pSep $ map PosArg [xpos, ypos]

fg :: Color -> Doc -> Doc
fg color content = dzen2DelimitedCommand FG mempty [ColorArg color] content

bg :: Color -> Doc -> Doc
bg color content = dzen2DelimitedCommand BG mempty [ColorArg color] content

i :: FilePath -> Doc
i iconFilePath = dzen2SimpleCommand I mempty [FilePathArg iconFilePath]

r :: Int -> Int -> Doc
r w h = dzen2SimpleCommand R dimensionSep $ map DimArg [w, h]

ro :: Int -> Int -> Doc
ro w h = dzen2SimpleCommand R dimensionSep $ map DimArg [w, h]

c :: Int -> Doc
c r = dzen2SimpleCommand C mempty [DimArg r]

co :: Int -> Doc
co r = dzen2SimpleCommand CO mempty [DimArg r]

ca :: MouseButton -> ShellCommand -> Doc -> Doc
ca mb sc content = dzen2DelimitedCommand CA clickableSep [MouseButtonArg mb, ShellCommandArg sc] content

toggleCollapse :: Doc 
toggleCollapse = dzen2SimpleCommand ToggleCollapse mempty []

collapse :: Doc
collapse = dzen2SimpleCommand Collapse mempty []

uncollapse :: Doc
uncollapse = dzen2SimpleCommand Uncollapse mempty []

toggleStick :: Doc
toggleStick = dzen2SimpleCommand ToggleStick mempty []

stick :: Doc
stick = dzen2SimpleCommand Stick mempty []

unstick :: Doc
unstick = dzen2SimpleCommand Unstick mempty []

toggleHide :: Doc
toggleHide = dzen2SimpleCommand ToggleHide mempty []

hide :: Doc
hide = dzen2SimpleCommand Hide mempty []

unhide :: Doc
unhide = dzen2SimpleCommand Unhide mempty []

raise :: Doc
raise = dzen2SimpleCommand Raise mempty []

lower :: Doc
lower = dzen2SimpleCommand Lower mempty []

scrollHome :: Doc
scrollHome = dzen2SimpleCommand ScrollHome mempty []

scrollEnd :: Doc
scrollEnd = dzen2SimpleCommand ScrollEnd mempty []

exit :: Doc
exit = dzen2SimpleCommand Exit mempty []

