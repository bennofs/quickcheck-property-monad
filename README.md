# quickcheck-property-monad

[![Build Status](https://secure.travis-ci.org/bennofs/quickcheck-property-monad.png?branch=master)](http://travis-ci.org/bennofs/quickcheck-property-monad)

## Introduction

When your data has many invariants, it's often difficult to write `Arbitrary` instances for QuickCheck. This library attempts to solve that
problem by providing a nice interface to write QuickCheck tests without using `Arbitrary` instances. It aims to be somewhere in the middle between
`HUnit` and `QuickCheck`: Use the random test case generation of `QuickCheck`, but write `HUnit` like assertions.

## A simple model

To show the library in action, let's first create a simple model holding an integer with a one invariant: the value hold by it will always be even.
We also provide two operations that preserve the invariant, `add2` and `multiply`, a function to create a new model and a function to check that the
invariant holds (we will use this function later when we write our tests):

```haskell
module Model
  ( Model() -- We don't export the constructor so the invariant cannot be broken.
  , newModel
  , add2
  , multiply
  , checkInvariant
  ) where

data Model = Model { value :: Int } deriving Show

newModel :: Model
newModel = Model 0

add2 :: Model -> Model
add2 (Model x) = Model $ 2 + x

multiply :: Int -> Model -> Model
multiply n (Model x) = Model $ n * x

checkInvariant :: Model -> Bool
checkInvariant (Model x) = even x
```

## Writing tests using the `PropM` monad

We now want to write tests that ensure that none of our operations will ever break the invariant, no matter in what sequence we apply them. In this case, we could
write an `Arbitrary` instance for our model, but for more complex models, this will quickly become very difficult. Often, you have to use the functions you want to test
to create the `Arbitrary` instance, which means that if the functions are broken, you already generate invalid data to begin with. It's difficult to find the bug in that case.

```haskell
module Main where

import Model
import Test.QuickCheck
import Test.QuickCheck.Property.Monad
```

But using `quickcheck-property-monad`, we can write the tests so that they will fail right after the invariant is broken. First, we define a `Gen` to generate a random
operation. We will also include a short description of the operation, to make debugging easier:

```haskell
randomOperation :: Gen (String, Model -> Model)
randomOperation = oneof
  [ return ("Add two", add2)
  , fmap (\n -> ("Multiply by " ++ show n, multiply n)) arbitrary
  ]
```

So far, all functions we've used are provided by `QuickCheck` itself. Let's now write the test property:

```haskell
prop_satisfies_invariant :: Property
prop_satisfies_invariant = sized $ \s -> property $ go newModel s
  where go :: Model -> Int -> PropM Bool
        go _     0    = return True
        go model size = do
          (description, operation) <- generate randomOperation
          logMessageLn $ "Operation: " ++ description
          let model' = operation model
          logMessageLn $ "Model is now: " ++ show model'
          assert "Number is even" $ checkInvariant model'
          go model' $ pred size
```

Here we've used some functions from `quickcheck-property-monad`. We first grab the `size` parameter from QuickCheck using `sized`, and then
pass that to go, together with an initial model. `go` returns a value of type `PropM Bool`, which we have to convert into a QuickCheck `Property`
using the `property` function.

But what does go do? First, it looks at the size parameter. If the size is null, we return `True`, indicating a successful test. If the size is not
null, we first generate a random operation, using our previously defined `randomOperation` function. We use `generate` to lift the `Gen` into the `PropM`
monad. After we generated a random operation, we log it. You'll see all messages logged with `logMessageLn` when the test fails, which is useful for
debugging. We then apply the operation on the model. Using `assert`, we require that the model still satisfies our invariant. `assert` will do nothing
if the condition given to it is True. If it is False, it will abort the test case and report a failure, with the given error message. After that, we recurse,
decreasing the size by one so that we eventually reach 0 and stop.

Now, the only function left to write is main:

```haskell
main :: IO ()
main = quickCheck prop_satisfies_invariant
```

If we run our test suite, we get the expected output:

```
+++ OK, passed 100 tests.
```

All fine!

## How a failure looks

Now, if you want to see how a failing test looks like, go back and change

```haskell
add2 (Model x) = Model $ 2 + x
```

to

```haskell
add2 (Model x) = Model $ 1 + x
```

If we now run our tests, we get a failure, as expected:

    *** Failed! Falsifiable (after 2 tests): 
    Operation: Add two
    Model is now: Model {value = 1}

    Assertion failed: Number is even

We get the output from the `logMessageLn` calls and the message of the assertion that failed.

## Contributing

Contributions are always welcome. If you have any ideas, improvements or bug reports,
send a pull request or open a issue on github.
