% Dependent Typing Haskell
% Dominic Steinitz
% 11th December 2015

---
bibliography: Book.bib
---

Conor McBride was not joking when he and his co-author entitled their
paper about dependent typing in Haskell "Hasochism":
@Lindley:2013:HPP:2503778.2503786.

In trying to resurrect the Haskell package
[yarr](http://www.stackage.org/package/yarr), it seemed that a
dependently typed reverse function needed to be written.

Writing such a function turns out to be far from straightforward. How
GHC determines that a proof (program) discharges a proposition (type
signature) is rather opaque and perhaps not surprisingly the error
messages one gets if the proof is incorrect are far from easy to
interpret.

Here are two implementations, each starting from different axioms (NB:
I have never seen type families referred to as axioms but it seems
helpful to think about them in this way).

> {-# OPTIONS_GHC -Wall                     #-}
> {-# OPTIONS_GHC -fno-warn-name-shadowing  #-}
> {-# OPTIONS_GHC -fno-warn-type-defaults   #-}
> {-# OPTIONS_GHC -fno-warn-unused-do-bind  #-}
> {-# OPTIONS_GHC -fno-warn-missing-methods #-}
> {-# OPTIONS_GHC -fno-warn-orphans         #-}

> {-# LANGUAGE GADTs                #-}
> {-# LANGUAGE KindSignatures       #-}
> {-# LANGUAGE DataKinds            #-}
> {-# LANGUAGE TypeFamilies         #-}
> {-# LANGUAGE UndecidableInstances #-}
> {-# LANGUAGE ExplicitForAll       #-}

For both implementations, we need propositional equality: if `a :~:
b`{.haskell} is inhabited by some terminating value, then the type
`a`{.haskell} is the same as the type `b`{.haskell}. Further we need
the normal form of an equality proof: `Refl :: a :~: a`{.haskell} and
a function, `gcastWith`{.haskell} which allows us to use internal
equality `(:~:)`{.haskell} to discharge a required proof
of external equality `(~)`{.haskell}. Readers familiar with topos
theory, for example see @lambek1988introduction, will note that the
notation is reversed.

> import Data.Type.Equality ( (:~:) (Refl), gcastWith )

The usual natural numbers:

> data Nat = Z | S Nat

We need some axioms:

1. A left unit and
2. Restricted commutativity.

> type family (n :: Nat) :+ (m :: Nat) :: Nat where
>     Z   :+ m = m
>     S n :+ m = n :+ S m

We need the usual singleton for `Nat`{.haskell} to tie types and terms together.

> data SNat :: Nat -> * where
>   SZero :: SNat Z
>   SSucc :: SNat n -> SNat (S n)

Now we can prove some lemmas.

> succ_plus_id :: SNat n1 -> SNat n2 -> (((S n1) :+ n2) :~: (S (n1 :+ n2)))
> succ_plus_id SZero _ = Refl
> succ_plus_id (SSucc n) m = gcastWith (succ_plus_id n (SSucc m)) Refl

This looks nothing like a standard mathematical proof and it's hard to
know what ghc is doing under the covers but presumably something like
this:

* For `SZero`{.haskell}
1. `S Z :+ n2 = Z :+ S n2`{.haskell} (by axiom 2) = `S n2`{.haskell} (by axiom 1) and
2. `S (Z + n2) = S n2`{.haskell} (by axiom 1)
3. So `S Z :+ n2 = S (Z + n2)`{.haskell}

* For `SSucc`{.haskell}
1. `SSucc n :: SNat (S k)`{.haskell} so `n :: SNat k`{.haskell} and `m :: SNat i so SSucc m :: SNat (S i)`{.haskell}
2. `succ_plus id n (SSucc m) :: k ~ S p => S p :+ S i :~: S (p :+ S i)`{.haskell} (by hypothesis)
3. `k ~ S p => S p :+ S i :~: S (S p :+ i)`{.haskell} (by axiom 2)
4. `k :+ S i :~: S (k :+ i)`{.haskell} (by substitution)
5. `S k :+   i :~: S (k :+ i)`{.haskell} (by axiom 2)



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

Bibliography
------------