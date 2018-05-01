{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.State (MonadState, StateT, evalStateT)
import Control.Exception.Safe (MonadThrow, MonadCatch)

type R = Int

type Game = Int


newtype App a = App (ReaderT R (StateT Game IO) a)
    deriving ( Functor, Applicative, Monad, MonadReader R, MonadIO, MonadState Game
             , MonadThrow, MonadCatch)

runApp :: R -> Game -> App a -> IO a
runApp r g (App m) = evalStateT (runReaderT m r) g

main = putStrLn "Hi"


