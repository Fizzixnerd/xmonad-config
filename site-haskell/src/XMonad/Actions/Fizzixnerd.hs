module XMonad.Actions.Fizzixnerd where

import XMonad

import System.Desktop.Commands.Fizzixnerd

waitHalfSecond :: X ()
waitHalfSecond = do
  spawn "sleep 0.5"

runBrowser :: X ()
runBrowser = do
  spawn browser

runExplorer :: X ()
runExplorer = do
  spawn explorer

runSystemMonitor :: X ()
runSystemMonitor = do
  spawn systemMonitor

runVideo :: X ()
runVideo = do
  spawn video

runMusic :: X ()
runMusic = do
  spawn music

runTerminal :: X ()
runTerminal = do
  spawn myTerminal

runEnableHorizontalTwoFingerScrolling :: X ()
runEnableHorizontalTwoFingerScrolling = do
  spawn enableHorizontalTwoFingerScrolling

runCompton :: X ()
runCompton = do
  spawn compton

runGnomeDo :: X ()
runGnomeDo = do
  spawn gnomeDo

runDock :: X ()
runDock = do
  spawn dock

runVolumeUp :: X ()
runVolumeUp = do
  spawn volumeUp

runVolumeDown :: X ()
runVolumeDown = do
  spawn volumeDown

runToggleMute :: X ()
runToggleMute = do
  spawn toggleMute
