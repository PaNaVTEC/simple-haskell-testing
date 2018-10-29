{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}

module Program where

import Control.Monad.State

-- Simple IO
userInteraction :: UserInteraction m => m ()
userInteraction = do
  response <- askUser
  tellUser $ "You said: " ++ response

class Monad m => UserInteraction m where
  askUser :: m String
  tellUser :: String -> m ()

instance UserInteraction IO where
  askUser :: IO String
  askUser = getLine

  tellUser :: String -> IO ()
  tellUser = putStrLn

-- Storage
increment :: (Num s, MonadState s m) => m ()
increment = do
  current <- get
  _ <- put (current + 1)
  return ()

data Transaction = Transaction
  {
    amount :: Int,
    desc :: String
  } deriving (Eq, Show)

storeList :: (MonadState [Transaction] m) => Transaction -> m ()
storeList t = do
  currentTransactions <- get
  _ <- put (t : currentTransactions)
  return ()
