dzenFont :: String
dzenFont = "Droid Sans Mono:style=Regular"

myDzenConfig = Dzen.font dzenFont

dzen2Time :: Logger
dzen2Time = dzenColorL "green" "black" $ logCmd time



dzen2PP = byorgeyPP {
  ppExtras = [dzen2Time, battery]
}
  
dzen2 config = statusBar "dzen2" dzen2PP (const (mod4Mask, xK_P)) config
