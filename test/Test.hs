{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -fplugin=DetectUnquantifiedTyVars #-}
module Test where


identity :: forall a. a -> a
identity = (\x -> x) :: b -> b

const :: a -> b -> a
const = (\x _ -> x) :: c -> b -> c

fun :: forall e f. (e -> f) -> [e] -> [f]
fun (ef :: e -> f) (es :: [x]) = fs
  where
    fs :: [f]
    fs = map ef es

fstInt :: (Int, Int) -> Int
fstInt (a, _) = a

class Eq1 f where
    eq1 :: Eq a => f a -> f a -> Bool

class Eq1 f => Ord1 f where
    compare1 :: forall a. Ord a => f a -> f a -> Ordering
