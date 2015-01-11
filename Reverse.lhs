% Dependent Typing Haskell
% Dominic Steinitz
% 28th December 2014

---
bibliography: Book.bib
---

Conor McBride was not joking when he and his co-author entitled their
paper about dependent typing in Haskell "Hasochism":
@Lindley:2013:HPP:2503778.2503786.

In trying to resurrect the Haskell package
[yarr](http://www.stackage.org/package/yarr), it seemed that a
dependently type reverse function needed to be written.

> {-# LANGUAGE GADTs                #-}
> {-# LANGUAGE KindSignatures       #-}
> {-# LANGUAGE DataKinds            #-}
> {-# LANGUAGE TypeFamilies         #-}
> {-# LANGUAGE UndecidableInstances #-}
> {-# LANGUAGE ExplicitForAll #-}

> import Data.Type.Equality

> data Nat = Z | S Nat

> type family (n :: Nat) :+ (m :: Nat) :: Nat where
>     Z   :+ m = m
>     S n :+ m = n :+ S m

> -- Singleton for Nat
> data SNat :: Nat -> * where
>   SZero :: SNat Z
>   SSucc :: SNat n -> SNat (S n)

> succ_plus_id :: SNat n1 -> SNat n2 -> (((S n1) :+ n2) :~: (S (n1 :+ n2)))
> succ_plus_id SZero _ = Refl
> succ_plus_id (SSucc n) m = gcastWith (succ_plus_id n (SSucc m)) Refl

> plus_id_r :: SNat n -> ((n :+ Z) :~: n)
> plus_id_r SZero = Refl
> plus_id_r (SSucc x) = gcastWith (plus_id_r x) (succ_plus_id x SZero)

> infixr 4 :::

> data Vec a n where
>     Nil   :: Vec a Z
>     (:::) :: a -> Vec a n -> Vec a (S n)

> size :: Vec a n -> SNat n
> size Nil         = SZero
> size (_ ::: xs)  = SSucc $ size xs

> elim0 :: SNat n -> (Vec a (n :+ Z) -> Vec a n)
> elim0 n x = gcastWith (plus_id_r n) x

> accrev :: Vec a n -> Vec a n
> accrev x = elim0 (size x) $ go Nil x where
>     go :: Vec a m -> Vec a n -> Vec a (n :+ m)
>     go acc  Nil       = acc
>     go acc (x ::: xs) = go (x ::: acc) xs

> safeHead :: Vec a (S n) -> a
> safeHead (x ::: _) = x

> toList :: Vec a n -> [a]
> toList  Nil       = []
> toList (x ::: xs) = x : toList xs

> test0 = toList $ accrev $ "a" ::: "b" ::: "c" ::: Nil
> test1 = safeHead $ accrev  $ "a" ::: "b" ::: "c" ::: Nil