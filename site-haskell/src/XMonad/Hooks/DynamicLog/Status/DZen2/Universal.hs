{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module XMonad.Hooks.DynamicLog.Status.DZen2.Universal where

import XMonad.Hooks.DynamicLog.Status.System
import XMonad.Hooks.DynamicLog.Status.StatusText

import Control.Monad.IO.Class

import Data.List
import Data.String
import Data.Monoid

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

-- | The Text representations of all the commands.

escape :: String
escape = "^"

pText :: String
pText = escape <> "p"

paText :: String
paText = escape <> "pa"

fgText :: String
fgText = escape <> "fg"

bgText :: String
bgText = escape <> "bg"

iText :: String
iText = escape <> "i"

rText :: String
rText = escape <> "r"

roText :: String
roText = escape <> "ro"

cText :: String
cText = escape <> "c"

coText :: String
coText = escape <> "co"

caText :: String
caText = escape <> "ca"

toggleCollapseText :: String
toggleCollapseText = escape <> "togglecollapse"

collapseText :: String
collapseText = escape <> "collapse"

uncollapseText :: String
uncollapseText = escape <> "uncollapse"

toggleStickText :: String
toggleStickText = escape <> "togglestick"

stickText :: String
stickText = escape <> "stick"

unstickText :: String
unstickText = escape <> "unstick"

toggleHideText :: String
toggleHideText = escape <> "togglehide"

hideText :: String
hideText = escape <> "hide"

unhideText :: String
unhideText = escape <> "unhide"

raiseText :: String
raiseText = escape <> "raise"

lowerText :: String
lowerText = escape <> "lower"

scrollHomeText :: String
scrollHomeText = escape <> "scrollhome"

scrollEndText :: String
scrollEndText = escape <> "scrollend"

exitText :: String
exitText = escape <> "exit"

-- | Return a Text representation of Command.
ppCommand :: Command -> String
ppCommand P = pText
ppCommand PA = paText
ppCommand FG = fgText
ppCommand BG = bgText
ppCommand I = iText
ppCommand R = rText
ppCommand RO = roText
ppCommand C = cText
ppCommand CO = coText
ppCommand CA = caText
ppCommand ToggleCollapse = toggleCollapseText
ppCommand Collapse = collapseText
ppCommand Uncollapse = uncollapseText
ppCommand ToggleStick = toggleStickText
ppCommand Stick = stickText
ppCommand Unstick = unstickText
ppCommand ToggleHide = toggleHideText
ppCommand Hide = hideText
ppCommand Unhide = unhideText
ppCommand Raise = raiseText
ppCommand Lower = lowerText
ppCommand ScrollHome = scrollHomeText
ppCommand ScrollEnd = scrollEndText
ppCommand Exit = exitText

ppPosition :: Position -> String
ppPosition HERE = mempty
ppPosition RESET_Y = mempty
ppPosition p = fromString $ show p

ppMouseButton :: MouseButton -> String
ppMouseButton LeftMB = "1"
ppMouseButton RightMB = "2"
ppMouseButton MiddleMB = "3"

ppShellCommand :: ShellCommand -> String
ppShellCommand sc = sc

ppArgument :: Argument -> String
ppArgument (PosArg p) = ppPosition p
ppArgument (ColorArg c) = c
ppArgument (FilePathArg fp) = fromString fp
ppArgument (DimArg i) = fromString $ show i
ppArgument (MouseButtonArg mb) = ppMouseButton mb
ppArgument (ShellCommandArg sc) = ppShellCommand sc

delimitedCommand :: String -> String -> [String] -> String -> String -> StatusText
delimitedCommand com sep args enclosed closer = makeStatusText opener [closer] enclosed 
  where 
    opener = fst $ tags $ simpleCommand com sep args

dzen2DelimitedCommand :: Command -> String -> [Argument] -> String -> StatusText
dzen2DelimitedCommand com sep args enclosed = 
  delimitedCommand (ppCommand com) sep (map ppArgument args) enclosed closer
  where
    closer = mconcat . fst . tags $ simpleCommand (ppCommand com) sep []

simpleCommand :: String -> String -> [String] -> StatusText
simpleCommand com sep args = makeStatusText [command] mempty mempty
  where
    command = com <> (parens $ mconcat $ intersperse sep args)
    parens x = "(" <> x <> ")"

dzen2SimpleCommand :: Command -> String -> [Argument] -> StatusText
dzen2SimpleCommand com sep args = simpleCommand (ppCommand com) sep $ map ppArgument args

pSep :: String
pSep = ";"

dimensionSep :: String
dimensionSep = "x"

clickableSep :: String
clickableSep = ","

-- DZen2 Commands

p :: Position -> Position -> StatusText
p xpos ypos = dzen2SimpleCommand P pSep $ map PosArg [xpos, ypos]

pa :: Position -> Position -> StatusText
pa xpos ypos = dzen2SimpleCommand PA pSep $ map PosArg [xpos, ypos]

fg :: Color -> String -> StatusText
fg color content = dzen2DelimitedCommand FG mempty [ColorArg color] content

bg :: Color -> String -> StatusText
bg color content = dzen2DelimitedCommand BG mempty [ColorArg color] content

i :: FilePath -> StatusText
i iconFilePath = dzen2SimpleCommand I mempty [FilePathArg iconFilePath]

r :: Int -> Int -> StatusText
r w h = dzen2SimpleCommand R dimensionSep $ map DimArg [w, h]

ro :: Int -> Int -> StatusText
ro w h = dzen2SimpleCommand R dimensionSep $ map DimArg [w, h]

c :: Int -> StatusText
c r = dzen2SimpleCommand C mempty [DimArg r]

co :: Int -> StatusText
co r = dzen2SimpleCommand CO mempty [DimArg r]

ca :: MouseButton -> ShellCommand -> String -> StatusText
ca mb sc content = 
  dzen2DelimitedCommand CA clickableSep [MouseButtonArg mb, ShellCommandArg sc] content

toggleCollapse :: StatusText
toggleCollapse = dzen2SimpleCommand ToggleCollapse mempty []

collapse :: StatusText
collapse = dzen2SimpleCommand Collapse mempty []

uncollapse :: StatusText
uncollapse = dzen2SimpleCommand Uncollapse mempty []

toggleStick :: StatusText
toggleStick = dzen2SimpleCommand ToggleStick mempty []

stick :: StatusText
stick = dzen2SimpleCommand Stick mempty []

unstick :: StatusText
unstick = dzen2SimpleCommand Unstick mempty []

toggleHide :: StatusText
toggleHide = dzen2SimpleCommand ToggleHide mempty []

hide :: StatusText
hide = dzen2SimpleCommand Hide mempty []

unhide :: StatusText
unhide = dzen2SimpleCommand Unhide mempty []

raise :: StatusText
raise = dzen2SimpleCommand Raise mempty []

lower :: StatusText
lower = dzen2SimpleCommand Lower mempty []

scrollHome :: StatusText
scrollHome = dzen2SimpleCommand ScrollHome mempty []

scrollEnd :: StatusText
scrollEnd = dzen2SimpleCommand ScrollEnd mempty []

exit :: StatusText
exit = dzen2SimpleCommand Exit mempty []
