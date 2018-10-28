{-# LANGUAGE InstanceSigs #-}

module Program where

import Data.Char
import Control.Monad.Trans

mainio :: IO ()
mainio = liftIO program

program :: UserInteraction m => m ()
program = do
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
