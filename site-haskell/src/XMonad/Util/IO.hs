module XMonad.Util.IO where

import Control.Monad.State
import Control.Monad.Reader

import XMonad

-- helper :: X a -> (XConf, XState, X a)
-- helper x = 
--   let y = do
--         xConf  <- ask
--         xState <- get
        
                 
  

-- liftToIO :: X a -> IO a
-- liftToIO x = do
--   xConf  <- ask
--   xState <- get
--   (val, newState) <- runX xConf xState x
--   put newState 
--   return val

-- inX :: (IO a -> b) -> X a -> (ReaderT r (StateT s IO) b)
-- inX f x = do
--   xconf <- ask
--   xstate <- get
--   (newState,resultFromIO) <- liftIO $ do
--     (valueFromX,nextState) <- runX xconf xstate x
--     return (nextState,f (return valueFromX))
--   put newState
--   return resultFromIO
