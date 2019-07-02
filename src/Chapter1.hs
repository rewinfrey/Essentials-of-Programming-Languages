module Chapter1 where

import Data.Typeable -- Used for value reflection to reify the type to check for inclusion in the set of Int.

{-

Definition 1.1.1 (top-down definition)

A natural number n is in S if and only if

1. n = 0, or

2. n - 3 ∈ S

Based on the two properties for inclusion in S, we can reason that any multiple of 3 is in S.

n = 1 is not in S because 1 != 0, and 1 - 3 = -2 and -2 is not a natural number.

n = 2 is not in S because 2 != 0, and 2 - 3 = -1 and -1 is not a natural number.

n = 3 is in S because 3 - 3 = 0 and 0 is in S.

Therefore, 0 or any natural number that is a multiple of 3 is in S.

-}

inS :: Int -> Bool
inS n | n == 0             = True
      | n >= 3, inS(n - 3) = True
      | otherwise          = False

{-

Definition 1.1.2 (bottom-up definition)

Define the set S to be the smallest set contained in N and satisfying the following two properties:

1. 0 ∈ S, and

2. if n ∈ S, then n + 3 ∈ S.

A "smallest set" is any set that satisfies the given properties and is a subset of any other set also satisfying the given properties.

There can only be on such "smallest set" in this case. If S1 and S2 both satisfy the properties, then S1 ⊆ S2, and S2 ⊆ S1, therefore S1 = S2.

-}

inS' :: Int -> Bool
inS' n | n == 0               = True
       | n >= 3
       , (n + 3) `mod` 3 == 0 = True
       | otherwise            = False

{-

Rule of inference notation (rules-of-inference version)


-------------
    0 ∈ S


    n ∈ S
-------------
 (n + 3) ∈ S

Each entry is a "rule". The horizontal line can be read as "if-then".
The part above the line is called the antecedent or hypothesis.
The part below the line is called the consequent or conclusion.
If there are two or more hypotheses, they are combined with an implicit "and".
A rule with no hypothesis is called an axiom. It is more common to write axioms without the horizontal line (i.e. 0 ∈ S)

-}

{-

Definition 1.1.3 (list of ints, top-down)

A Scheme list is a list of ints if and only if either

1. it is the empty list, or

2. it is a pair whose car is an integer and whose cdr is a list of integers.

-}

-- In Haskell this is a trivial definition:
listOfInts :: [Int] -> Bool
listOfInts ls = True

-- But to make it more interesting, we can wrap each element in a newtype affording us a list of values whose types are heterogeneous:
newtype Wrapper a = Wrapper a

listOfInts' :: Typeable a => [Wrapper a] -> Bool
listOfInts' [] = True
listOfInts' (a:as) | isInt a, listOfInts' as = True
                   | otherwise = False
  where
    -- This is the simplest way I've found to reflect on the type representation of a term. Because of the `newtype` wrapper,
    -- a `Wrapper (1 :: Int)` is representationally equal to `1 :: Int`, but is not nominally equal.
    -- In order to test for nominal equality (meaning the names of the types match) we have to discard the `newtype` wrapper.
    isInt (Wrapper a) = typeOf a == typeOf (1 :: Int)

{-

Definition 1.1.4 (list of ints, bottom-up)

The set List-of-Int is the smallest set of Scheme lists satisfying the following two properties:

1. () ∈ List-of-Int, and

2. if n ∈ Int and l ∈ List-of-Int, then (n . l) ∈ List-of-Int

Here, `.` is used to denote the `cons` operator in Scheme (rather than Haskell's composition operator).

-}

{-

Definition 1.1.5 (list of ints, rules of inference)

() ∈ List-of-Int

  n ∈ Int   l ∈ List-of-Int
-----------------------------
    (n . l) ∈ List-of-Int

-}

-- Exercises
