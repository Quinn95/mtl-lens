{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.State (MonadState, StateT, evalStateT)
import Control.Exception.Safe (MonadThrow, MonadCatch)

type Config = Int

type GameState = Int


newtype App a = App (ReaderT Config (StateT GameState IO) a)
    deriving ( Functor, Applicative, Monad, MonadReader Config, MonadIO, MonadState GameState
             , MonadThrow, MonadCatch)

runApp :: Config -> GameState -> App a -> IO a
runApp r g (App m) = evalStateT (runReaderT m r) g

main = putStrLn "Hi"


