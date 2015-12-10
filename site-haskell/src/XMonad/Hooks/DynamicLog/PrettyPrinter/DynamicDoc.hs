{-# LANGUAGE MultiParamTypeClasses #-}

module XMonad.Hooks.DynamicLog.PrettyPrinter.DynamicDoc where

import XMonad
import XMonad.Util.Run

import Control.Monad.IO.Class

import Text.PrettyPrint hiding (empty)

import System.IO (FilePath, Handle, hPutStrLn)

type DynamicDoc m = m Doc
type Process = (FilePath, [String])

-- -- | Laws:
-- -- (i)   process $ f .$. x == f <$> process $ x
-- -- (ii)  id .$. x == x
-- -- (iii) (f . g) .$. x == f .$. (g .$. x)
-- class Processable c a where
--   (.$.)   :: Magma c => (a -> a) -> c -> c
--   process :: (MonadIO m, Magma c) => c -> m a

-- class Magma a where
--   (.+.) :: a -> a -> a
--   magSum :: Container a => [a] -> a
--   magSum = foldl (\acc x -> acc .+. x) empty

-- class Container a where
--   empty :: a

-- instance Magma DynamicDoc where
--   x .+. y = Compound ([x, y], id)

-- instance Container DynamicDoc where
--   empty = Atom (DynDoc mempty, id)

-- instance Processable DynamicDoc Doc where
--   f .$. (Atom     (d, g)) = Atom     (d, f . g)
--   f .$. (Compound (d, g)) = Compound (d, f . g)
--   process                 = compileDynamicDoc

dynStr :: (Monad m) => String -> DynamicDoc m
dynStr = return . text

dynDoc :: (Monad m) => Doc -> DynamicDoc m
dynDoc = return

dynProc :: (MonadIO m) => Process -> DynamicDoc m
dynProc (process, args) = text <$> runProcessWithInput process args ""

dynAct :: (Monad m) => (a -> Doc) -> m a -> DynamicDoc m
dynAct f x = f <$> x

renderOneLine :: Doc -> String
renderOneLine = renderStyle oneLineStyle

oneLineStyle :: Style
oneLineStyle = style 
               {
                 mode = OneLineMode
               , lineLength = maxBound
               }

dynamicRenderOneLine :: (Monad m) => DynamicDoc m -> m String
dynamicRenderOneLine = fmap renderOneLine

fromStrF :: (Monad m) => (String -> String) -> Style -> (DynamicDoc m -> DynamicDoc m)
fromStrF f styl = fmap (text . f . (renderStyle styl))

fromStrFOL :: (Monad m) => (String -> String) -> (DynamicDoc m -> DynamicDoc m)
fromStrFOL f = fromStrF f oneLineStyle

hPrintDynamicDoc :: (MonadIO m) => Handle -> DynamicDoc m -> m ()
hPrintDynamicDoc h d = do
  str <- dynamicRenderOneLine d
  liftIO $ hPutStrLn h str
