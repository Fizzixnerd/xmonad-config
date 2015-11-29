-- | This defines a bunch of generic commands that can be used in a
-- variety of X actions.

module System.Desktop.Commands.Fizzixnerd where

browser = "firefox"
explorer = "nautilus"
systemMonitor = "gnome-system-monitor"
music = "rhythmbox"
video = "vlc"
myTerminal = "gnome-terminal"

compton = "killall compton; sleep 0.5; compton -f -I 0.10 -O 0.10 --backend glx --vsync opengl"
dock = "killall docky; sleep 0.5; docky"
gnomeDo = "gnome-do"
enableHorizontalTwoFingerScrolling = "synclient HorizTwoFingerScroll=1"

volumeUp = "amixer -D pulse sset Master 5%+"
volumeDown = "amixer -D pulse sset Master 5%-"
toggleMute = "amixer -D pulse set Master 1+ toggle"

time = "date"
