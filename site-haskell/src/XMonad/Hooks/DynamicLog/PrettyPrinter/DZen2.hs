{-# LANGUAGE OverloadedStrings #-}

module XMonad.Hooks.DynamicLog.PrettyPrinter.DZen2 where

import Text.PrettyPrint hiding (sep)

import Data.List

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
data Argument = PosArg Position | ColorArg Color | FilePathArg FilePath | DimArg Int | MouseButtonArg MouseButton | ShellCommandArg ShellCommand

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
ppPosition HERE = empty
ppPosition RESET_Y = empty
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
fg color content = dzen2DelimitedCommand FG empty [ColorArg color] content

bg :: Color -> Doc -> Doc
bg color content = dzen2DelimitedCommand BG empty [ColorArg color] content

i :: FilePath -> Doc
i iconFilePath = dzen2SimpleCommand I empty [FilePathArg iconFilePath]

r :: Int -> Int -> Doc
r w h = dzen2SimpleCommand R dimensionSep $ map DimArg [w, h]

ro :: Int -> Int -> Doc
ro w h = dzen2SimpleCommand R dimensionSep $ map DimArg [w, h]

c :: Int -> Doc
c r = dzen2SimpleCommand C empty [DimArg r]

co :: Int -> Doc
co r = dzen2SimpleCommand CO empty [DimArg r]

ca :: MouseButton -> ShellCommand -> Doc -> Doc
ca mb sc content = dzen2DelimitedCommand CA clickableSep [MouseButtonArg mb, ShellCommandArg sc] content

toggleCollapse :: Doc 
toggleCollapse = dzen2SimpleCommand ToggleCollapse empty []

collapse :: Doc
collapse = dzen2SimpleCommand Collapse empty []

uncollapse :: Doc
uncollapse = dzen2SimpleCommand Uncollapse empty []

toggleStick :: Doc
toggleStick = dzen2SimpleCommand ToggleStick empty []

stick :: Doc
stick = dzen2SimpleCommand Stick empty []

unstick :: Doc
unstick = dzen2SimpleCommand Unstick empty []

toggleHide :: Doc
toggleHide = dzen2SimpleCommand ToggleHide empty []

hide :: Doc
hide = dzen2SimpleCommand Hide empty []

unhide :: Doc
unhide = dzen2SimpleCommand Unhide empty []

raise :: Doc
raise = dzen2SimpleCommand Raise empty []

lower :: Doc
lower = dzen2SimpleCommand Lower empty []

scrollHome :: Doc
scrollHome = dzen2SimpleCommand ScrollHome empty []

scrollEnd :: Doc
scrollEnd = dzen2SimpleCommand ScrollEnd empty []

exit :: Doc
exit = dzen2SimpleCommand Exit empty []
