{-# LANGUAGE MultiParamTypeClasses #-}

module XMonad.Hooks.DynamicLog.PrettyPrinter.DynamicDoc where

import XMonad.Util.Run

import Control.Monad.IO.Class

import Text.PrettyPrint hiding (empty)

import System.IO

type Process = (FilePath, [String])

data DynDoc = DynDoc Doc
            | DynProc Process deriving (Show, Eq)

data DynamicDoc = Atom (DynDoc, Doc -> Doc)
                | Compound ([DynamicDoc], Doc -> Doc)

-- | Laws:
-- (i)   process $ f .$. x == f <$> process $ x
-- (ii)  id .$. x == x
-- (iii) (f . g) .$. x == f .$. (g .$. x)
class Processable c a where
  (.$.)   :: Magma c => (a -> a) -> c -> c
  process :: (MonadIO m, Magma c) => c -> m a

class Magma a where
  (.+.) :: a -> a -> a
  magSum :: Container a => [a] -> a
  magSum = foldl (\acc x -> acc .+. x) empty

class Container a where
  empty :: a

instance Magma DynamicDoc where
  x .+. y = Compound ([x, y], id)

instance Container DynamicDoc where
  empty = Atom (DynDoc mempty, id)

instance Processable DynamicDoc Doc where
  f .$. (Atom     (d, g)) = Atom     (d, f . g)
  f .$. (Compound (d, g)) = Compound (d, f . g)
  process                 = compileDynamicDoc

dynStr :: String -> DynamicDoc
dynStr x = Atom (DynDoc $ text x, id)

dynDoc :: Doc -> DynamicDoc
dynDoc x = Atom (DynDoc x, id)

dynProc :: Process -> DynamicDoc
dynProc x = Atom (DynProc x, id)

runProcess :: MonadIO m => Process -> m String
runProcess (process, args) = runProcessWithInput process args ""

compileDynDoc :: MonadIO m => DynDoc -> m Doc
compileDynDoc (DynDoc  d) = return d
compileDynDoc (DynProc p) = text <$> runProcess p 

compileDynamicDoc :: MonadIO m => DynamicDoc -> m Doc
compileDynamicDoc (Atom      (d, f)) = f <$> compileDynDoc d
compileDynamicDoc (Compound (ds, f)) = f . mconcat <$> mapM compileDynamicDoc ds

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
dynamicRenderStatusBar d = renderStatusBar <$> compileDynamicDoc d

hPrintDynamicDoc :: Handle -> DynamicDoc -> IO ()
hPrintDynamicDoc h d = do
  out <- dynamicRenderStatusBar d
  hPutStrLn h out

