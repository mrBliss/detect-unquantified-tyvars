{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
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
    id2 :: a -> a
    id2 x = x
    local :: forall t u. t -> u -> t
    local (z :: t) _ = z :: t
      where
        id3 :: b -> b
        id3 x = let id4 :: c -> c
                    id4 y = y
                    z' :: t
                    z' = z
                in x

rank :: forall a. a -> a -> forall b. b -> b
rank (x :: c) (y :: a) z = z

fstInt :: (Int, Int) -> Int
fstInt (a, _) = a

mixed :: a -> forall b. b -> b
mixed _ x = x

shadow :: a -> forall a. a -> a
shadow _ x = x

multi, ple :: forall a b. a -> b -> a
multi x y = let f :: a -> a
                f z = z
            in f x
ple x y = let f :: t -> t
              f z = z
          in f x

class Eq1 f where
    eq1 :: Eq a => f a -> f a -> Bool

class Eq1 f => Ord1 f where
    compare1 :: forall a. Ord a => f a -> f a -> Ordering

data Id a = Id a

instance Eq1 Id where
    eq1 :: Eq a => Id a -> Id a -> Bool
    eq1 (Id x) (Id y) = x == y

data Pha p a = Pha a

instance Eq1 (Pha p) where
    eq1 :: forall a. Eq a => Pha p a -> Pha p a -> Bool
    eq1 (Pha x) (Pha y) = x == y
      where
        ida :: a -> a
        ida x = x
        idb :: b -> b
        idb x = x
