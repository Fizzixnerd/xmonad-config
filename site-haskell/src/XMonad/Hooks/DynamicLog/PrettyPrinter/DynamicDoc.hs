module XMonad.Hooks.DynamicLog.PrettyPrinter.DynamicDoc where

import Control.Monad.IO.Class

import XMonad.Util.Run

import Text.PrettyPrint

type Process = (FilePath, [String])

data DynamicDoc = DynDoc Doc
                | DynStr String
                | DynProc Process
                | CompoundDynDoc [DynamicDoc] deriving (Show)

runProcess :: MonadIO m => Process -> m String
runProcess (process, args) = runProcessWithInput process args ""

compileDynDoc :: MonadIO m => DynamicDoc -> m Doc
compileDynDoc (DynDoc d) = return d
compileDynDoc (DynStr s) = return $ text s
compileDynDoc (DynProc p) = fmap text $ runProcess p
compileDynDoc (CompoundDynDoc cdd) =  fmap (foldl (\acc x -> acc <> x) empty) $ sequence $ fmap compileDynDoc cdd

renderStatusBar :: Doc -> String
renderStatusBar = renderStyle style 
                  {
                    mode = OneLineMode
                  , lineLength = maxBound
                  }
