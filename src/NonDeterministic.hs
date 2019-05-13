module NonDeterministic where

import Control.Monad (guard)

-- |
-- https://stackoverflow.com/questions/29886852/why-is-the-following-haskell-code-non-deterministic
--
-- In this context, what's non-deterministic isn't the computation that Haskell is performing,
-- but rather the computation that is being represented.
--
-- If it helps, we can think of the list monad, or list comprehensions, as performing a sort of Cartesian product.
-- A different way of encoding this is to start an independent concurrent computation for each of the choices, at once,
-- producing the final results without any inherent order.
--
-- Prelude> nonDeterministicListMonad
-- [5,6,7,6,7,8]
--
-- Prelude> nonDeterministicListApplicative
-- [5,6,7,6,7,8]
--
-- Prelude> nonDeterministicListComprehension
-- [5,6,7,6,7,8]
--

xs = [1, 2]
ys = [4, 5, 6]

nonDeterministicListMonad = do
  x <- xs         -- if x is 1 or 2
  y <- ys         -- and y is 4, 5, or 6
  return (x + y)  -- then what are the possible values of x + y?

nonDeterministicListApplicative = (+) <$> xs <*> ys

nonDeterministicListComprehension = [x + y | x <- xs, y <- ys]

-- |
-- More references related to non-deterministic List monad
-- https://stackoverflow.com/questions/20638893/how-can-non-determinism-be-modeled-with-a-list-monad
-- https://softwareengineering.stackexchange.com/questions/363140/what-is-the-reasoning-behind-making-non-determinism-a-feature-of-haskell
-- https://stackoverflow.com/questions/27265920/what-is-non-determinism-in-haskell

data CoinType = Fair | Biased deriving (Show)

data Coin = Head | Tail deriving (Eq,Show)

toss Fair   = [Head, Tail]
toss Biased = [Head, Head]

pick = [Fair, Biased]

experiment = do
  coin   <- pick         -- Pick a coin at random
  result <- toss coin    -- Toss it, to get a result
  guard (result == Head) -- We only care about results that come up Heads
  return coin            -- Return which coin was used in this case

-- |
-- Prelude> experiment
-- [Fair,Biased,Biased]
