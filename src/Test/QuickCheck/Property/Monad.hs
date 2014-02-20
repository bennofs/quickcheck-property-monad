{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Tutorial: <http://github.com/bennofs/quickcheck-property-monad/tree/master/README.md>
module Test.QuickCheck.Property.Monad
  ( PropM()
  , assert
  , failWith
  , generate
  , logMessage
  , logMessageLn
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.Writer
import Test.QuickCheck.Gen
import Test.QuickCheck.Property

-- | PropM is a monad for writing properties that depend on random
-- data. This is especially useful if you have many invariants for
-- your data and cannot simply write an 'Arbitrary' instance.
newtype PropM a = PropM (EitherT String (WriterT String Gen) a) deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

instance Testable a => Testable (PropM a) where
  property (PropM m) = property $ do
    (r,w) <- runWriterT $ runEitherT m
    case r of
      Right r' -> return $ property r'
      Left err -> return $ printTestCase (w ++ "\n" ++ err) $ property False

-- | Assert that a certain condition is true. If the condition is false, fail with the
-- given error message.
assert :: String -> Bool -> PropM ()
assert err cond = unless cond $ failWith $ "Assertion failed: " ++ err

-- | Fail with the given error message.
failWith :: String -> PropM ()
failWith err = PropM $ left err

-- | Use the given generator to generate a value.
generate :: Gen a -> PropM a
generate = PropM . lift . lift

-- | Log a message that will be printed when the test case fails.
logMessage :: String -> PropM ()
logMessage = PropM . lift . tell

-- | Like 'logMessage' but appends a line break after the message.
logMessageLn :: String -> PropM ()
logMessageLn = logMessage . (++ "\n")
