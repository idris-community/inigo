module Extra.List

import Data.List
import Data.Maybe

export
dropPrefix : Eq a => List a -> List a -> List a
dropPrefix pre x =
  if isPrefixOf pre x
    then drop (length pre) x
    else x

export
collapse : Monad t => List (t a) -> t (List a)
collapse =
  foldl f (pure [])
    where
      fi : t a -> List a -> t (List a)
      fi el acc =
        map (\e => e :: acc) el

      f : t (List a) -> t a -> t (List a)
      f acc el =
        acc >>= fi el

export
range : Nat -> List Nat
range n =
  reverse (drop 1 (iterate next n))
    where
      next : Nat -> Maybe Nat
      next (S k) = Just k
      next Z = Nothing

export
zipWithIndex : List a -> List (a, Nat)
zipWithIndex l =
  zip l (range (length l))

export
repeat : a -> Nat -> List a
repeat _ Z = []
repeat x (S n) = x :: repeat x n

export
findAll : Eq a => List a -> List a -> List Nat
findAll l k =
  reverse (findAllHelper l k 0 [])
  where
    findAllHelper : List a -> List a -> Nat -> List Nat -> List Nat
    findAllHelper l k index acc =
      case l of
        [] =>
          acc
        _ =>
          let
            nextAcc =
              if isPrefixOf k l then
                (index :: acc)
              else
                acc
          in
            findAllHelper (drop 1 l) k (index + 1) nextAcc



export
replace : Eq a => List a -> List a -> List a -> List a
replace l k v =
  let
    matches = findAll l k
    repl = reverse v
    skipLen = case length k of
      Z =>
        0 -- This is really impossible but we haven't refined our types to say so
      (S Z) =>
        0
      (S k) =>
        k
  in
    reverse $ fst $ foldl (\(acc, skip), (c, i) =>
      case skip of
        Just (S k) =>
          (acc, Just k)
        _ =>
          if any (== i) matches then
            -- We need to insert a replacement and then skip N els
            (repl ++ acc, Just skipLen)
          else
            (c :: acc, Nothing)
    ) ([], (the (Maybe Nat) Nothing)) (zipWithIndex l)
