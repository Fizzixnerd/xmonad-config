module XMonad.Hooks.DynamicLog.PrettyPrinter.DynamicDoc where

import XMonad.Util.Run

import Control.Monad.IO.Class

import Text.PrettyPrint

import System.IO

type Process = (FilePath, [String])

data DynamicDoc = DynDoc Doc
                | DynStr String
                | DynProc Process
                | CompoundDynDoc [DynamicDoc] deriving (Show, Eq)

instance Monoid DynamicDoc where
  (CompoundDynDoc d) `mappend` (CompoundDynDoc e) = CompoundDynDoc $ d ++ e
  (CompoundDynDoc d) `mappend` x                  = CompoundDynDoc $ d ++ [x]
  x `mappend` (CompoundDynDoc d)                  = CompoundDynDoc $ x:d
  x `mappend` y                                   = CompoundDynDoc $ [x, y]
  mempty                                          = CompoundDynDoc $ []

runProcess :: MonadIO m => Process -> m String
runProcess (process, args) = runProcessWithInput process args ""

compileDynamicDoc :: MonadIO m => DynamicDoc -> m Doc
compileDynamicDoc (DynDoc d) = return d
compileDynamicDoc (DynStr s) = return $ text s
compileDynamicDoc (DynProc p) = fmap text $ runProcess p
compileDynamicDoc (CompoundDynDoc cdd) = fmap (foldl (\acc x -> acc <> x) empty) 
                                     $ sequence
                                     $ fmap compileDynamicDoc cdd

renderStatusBar :: Doc -> String
renderStatusBar = renderStyle style 
                  {
                    mode = OneLineMode
                  , lineLength = maxBound
                  }

dynamicRenderStatusBar :: MonadIO m => DynamicDoc -> m String
dynamicRenderStatusBar d = fmap renderStatusBar $ compileDynamicDoc d

hPrintDynamicDoc :: Handle -> DynamicDoc -> IO ()
hPrintDynamicDoc h d = do
  out <- dynamicRenderStatusBar d
  hPutStrLn h out

