{-# language DeriveFunctor #-}
{-# language GeneralizedNewtypeDeriving #-}

module ProgramSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Functor.Identity
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State

import Program

main :: IO ()
main = hspec spec

newtype TestM a = TestM
  {
    unTest :: ReaderT String (WriterT String Identity) a
  } deriving (Functor, Applicative, Monad, MonadReader String, MonadWriter String)

instance UserInteraction TestM where
  askUser = ask
  tellUser = tell

spec :: Spec
spec = do
  describe "User Interactions" $ do
    it "says something" $ do
      executeUserInteractionProgram "Something" `shouldBe` "You said: Something"

  describe "Store things" $ do
    it "increments a counter in state" $ do
      executeIncrementProgram 1 `shouldBe` 2

executeUserInteractionProgram :: String -> String
executeUserInteractionProgram = snd . runIdentity . runWriterT . (runReaderT $ unTest userInteraction)

executeIncrementProgram :: Int -> Int
executeIncrementProgram = snd . runIdentity . runStateT increment
