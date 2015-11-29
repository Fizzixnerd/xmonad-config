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
data Argument = PosArg Position

type ShellCommand = String

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
escape = zeroWidthText '^'

p :: Doc
p = escape <> zeroWidthText "p"

pa :: Doc
pa = escape <> zeroWidthText "pa"

fg :: Doc
fg = escape <> zeroWidthText "fg"

bg :: Doc
bg = escape <> zeroWidthText "bg"

i :: Doc
i = escape <> zeroWidthText "i"

r :: Doc
r = escape <> zeroWidthText "r"

ro :: Doc
ro = escape <> zeroWidthText "ro"

c :: Doc
c = escape <> zeroWidthText "c"

co :: Doc
co = escape <> zeroWidthText "co"

ca :: Doc
ca = escape <> zeroWidthText "ca"

toggleCollapse :: Doc
toggleCollapse = escape <> zeroWidthText "togglecollapse"

collapse :: Doc
collapse = escape <> zeroWidthText "collapse"

uncollapse :: Doc
uncollapse = escape <> zeroWidthText "uncollapse"

toggleStick :: Doc
toggleStick = escape <> zeroWidthText "togglestick"

stick :: Doc
stick = escape <> zeroWidthText "stick"

unstick :: Doc
unstick = escape <> zeroWidthText "unstick"

toggleHide :: Doc

posSep :: Doc
posSep = zeroWidthText ";"

ppCommand :: Command -> Doc
ppCommand P = p
ppCommand PA = pa
ppCommand FG = fg
ppCommand BG = bg
ppCommand I = i
ppCommand R = r
ppCommand RO = ro
ppCommand C = c
ppCommand CO = co
ppCommand CA = ca

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

command :: Doc -> Doc -> [Doc] -> Doc
command com sep args = com <> (parens $ hcat $ intersperse sep args)

dzen2Command :: Command -> Doc -> [Argument] -> Doc
dzen2Command com sep args = command (ppCommand com) sep $ map ppArgument args

posR :: Position -> Position -> Doc
posR xpos ypos = dzen2Command P posSep $ map PosArg [xpos, ypos]

posA :: Position -> Position -> Doc
posA xpos ypos = dzen2Command PA posSep $ map PosArg [xpos, ypos]

