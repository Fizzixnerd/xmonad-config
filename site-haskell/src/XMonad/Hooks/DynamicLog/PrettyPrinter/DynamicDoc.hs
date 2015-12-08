{-# LANGUAGE MultiParamTypeClasses #-}

module XMonad.Hooks.DynamicLog.PrettyPrinter.DynamicDoc where

import XMonad.Util.Run

import Control.Monad.IO.Class

import Text.PrettyPrint

import System.IO

type Process = (FilePath, [String])

data DynDoc = DynDoc Doc
            | DynProc Process deriving (Show, Eq)

data DynamicDoc = Atom (DynDoc, Doc -> Doc)
                | Compound ([DynamicDoc], Doc -> Doc)

-- | Laws:
-- (i)   process $ f .$. x = (process x) >>= f
-- (ii)  id .$. x = x
-- (iii) (f . g) .$. x = f .$. (g .$. x)
class Processable a b where
  (.$.)   :: Monoid w => (b -> b) -> w -> w
  process :: (Monad m, Monoid w) => w -> m b

instance Monoid DynamicDoc where
  x `mappend` y = Compound ([x, y], id)
  mempty        = Compound ([], id)

instance Processable DynamicDoc Doc where
  f .$. (Atom x)     = Atom     (fst x, f . (snd x))
  f .$. (Compound x) = Compound (fst x, f . (snd x))
  process            = compileDynamicDoc

runProcess :: MonadIO m => Process -> m String
runProcess (process, args) = runProcessWithInput process args ""

compileDynDoc :: MonadIO m => DynDoc -> m Doc
compileDynDoc (DynDoc d)  = return d
compileDynDoc (DynProc p) = text <$> runProcess p 

compileDynamicDoc :: MonadIO m => DynamicDoc -> m Doc
compileDynamicDoc (Atom (d, f)) = f <$> compileDynDoc d
compileDynamicDoc (Compound (ds, f)) = f <$> mconcat <$> sequence $ compileDynamicDoc <$> ds

-- compileDynamicDoc (DynDoc d) = return d
-- compileDynamicDoc (DynStr s) = return $ text s
-- compileDynamicDoc (DynProc p) = fmap text $ runProcess p
-- compileDynamicDoc (CompoundDynDoc cdd) = fmap (foldl (\acc x -> acc <> x) empty) 
--                                      $ sequence
--                                      $ fmap compileDynamicDoc cdd

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

