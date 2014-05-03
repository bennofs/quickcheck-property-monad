{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Tutorial: <http://github.com/bennofs/quickcheck-property-monad/tree/master/README.md>
--
-- Note about the examples: The examples use a "-" in place of the empty line. This is required in order for doctest
-- to work.
module Test.QuickCheck.Property.Monad
  ( PropM()
  , assert
  , failWith
  , gen
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

{- $setup
>>> :set -XNoMonomorphismRestriction
>>> import Test.QuickCheck (quickCheckWithResult, stdArgs, chatty, output, replay)
>>> import Test.QuickCheck.Random (mkQCGen)
>>> let transformEmpty "" = "-"; transformEmpty x = x
>>> let quickCheck = quickCheckWithResult (stdArgs { chatty = False, replay = Just (mkQCGen 42, 13) }) >=> putStr . unlines . map transformEmpty . lines . output
-}

-- | PropM is a monad for writing properties that depend on random
-- data. This is especially useful if you have many invariants for
-- your data and cannot simply write an 'Arbitrary' instance.
--
-- You can use a @PropM a@ as a QuickCheck Testable if @a@ is Testable. For example,
-- you can use @PropM Bool@ as a Testable property:
--
-- >>> quickCheck (return True :: PropM Bool)
-- +++ OK, passed 100 tests.
--
-- >>> quickCheck (return False :: PropM Bool)
-- *** Failed! Falsifiable (after 1 test):
-- -
newtype PropM a = PropM (EitherT String (WriterT String Gen) a) deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

instance Testable a => Testable (PropM a) where
  property (PropM m) = property $ fmap (counterexample . snd <*> makeProperty . fst) $ runWriterT $ runEitherT m where
    makeProperty (Right r) = property r
    makeProperty (Left err) = counterexample err $ property False

-- | Assert that a certain condition is true. If the condition is false, fail with the
-- given error message.
--
-- Examples:
--
-- >>> quickCheck $ assert "True is True!" True >> return True
-- +++ OK, passed 100 tests.
--
-- >>> quickCheck $ assert "False is True!" False >> return True
-- *** Failed! Falsifiable (after 1 test):
-- -
-- Assertion failed: False is True!
assert :: String -> Bool -> PropM ()
assert err cond = unless cond $ failWith $ "Assertion failed: " ++ err

-- | Fail with the given error message.
--
-- Example:
--
-- >>> quickCheck $ failWith "Something horrible happened" >> return True
-- *** Failed! Falsifiable (after 1 test):
-- -
-- Something horrible happened
failWith :: String -> PropM ()
failWith err = PropM $ left err

-- | Use the given generator to generate a value.
--
-- Examples:
--
-- >>> quickCheck $ fmap (`elem` [0..5]) $ gen (elements [0..5])
-- +++ OK, passed 100 tests.
--
-- >>> quickCheck $ fmap (> 0) $ gen (choose (0,1))
-- *** Failed! Falsifiable (after 2 tests):
-- -
gen :: Gen a -> PropM a
gen = PropM . lift . lift

-- | Log a message that will be printed when the test case fails.
logMessage :: String -> PropM ()
logMessage = PropM . lift . tell

-- | Like 'logMessage' but appends a line break after the message.
--
-- Example:
--
-- >>> quickCheck $ gen (choose (0,1)) >>= \x -> logMessageLn ("Chosen: " ++ show x) >> return (x > 0)
-- *** Failed! Falsifiable (after 2 tests):
-- Chosen: 0
-- -
logMessageLn :: String -> PropM ()
logMessageLn = logMessage . (++ "\n")
