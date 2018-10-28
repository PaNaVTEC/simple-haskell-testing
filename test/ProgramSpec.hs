{-# language DeriveFunctor #-}
{-# language GeneralizedNewtypeDeriving #-}
module ProgramSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Functor.Identity
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Writer

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
     executeProgramWith "Something" `shouldBe` "You said: Something"

executeProgramWith :: String -> String
executeProgramWith = snd . runIdentity . runWriterT . (runReaderT $ unTest program)
