module Lib
  ( someFunc
  ) where

import ClassyPrelude
import Control.Monad.Except
import Text.StringRandom
import Data.Has

data State = State
  { stateUserIdCounter :: String
  } deriving (Show, Eq)

type InMemory r m = (Has (TVar State) r, MonadReader r m, MonadIO m)

hello :: (MonadReader String m, MonadIO m) => m ()
hello = do
  name <- ask
  liftIO $ print $ "Hello," <> name <> "!"

newtype App a = App
  { unAppT :: ReaderT String IO a
  } deriving (Applicative, Functor, Monad, MonadReader String, MonadIO)



someFunc :: IO ()
--someFunc = putStrLn "someFunc"
someFunc = flip runReaderT "world" $ unAppT hello
