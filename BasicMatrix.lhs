
Introduction
============

The intended audience of this blog post (which will be a chapter or
part of a chapter in the book) is applied mathematicians,
computational physicists, numerical analysts and anyone who has the
need to compute numerical solutions, who have a limited acquaintance
with Haskell and who do not wish to learn about parsing, searching and
sorting, compiler design and other such techniques that are often used
as examples in pedagogic Haskell material. Instead we take the example
of multiplying matrices in the hope that it will provide a more
motivating example. This intitial version is based on a fascinating
discussion the author had with two (applied?) mathematicians. The
exposition is more detailed than other blog posts although more words
does not necessarily mean greater perspicuity. All feedback is
gratefully received.

Implementations
===============

We assume our matrix is dense. There are many ways to implement matrix
multiplication in Haskell. We start with the bookies' favourite.

Lists
-----

We represent vectors as lists and matrices as lists of vectors, that
is lists of lists. We can capture this using type synonyms.

> type Vector a = [a]
> type Matrix a = [Vector a]

First let us multiply a vector by a matrix. Let us ignore the
implementation to start with and just focus on the type
signature. First we note that Haskell can infer this type; we do not
actually need to give it although this is a good check the
implementation in some sense satisfies its specification.

> matTimesVecNoSig m v = result
>   where
>     lrs = map length m
>     l   = length v
>     result = if all (== l) lrs
>              then map (\r -> sum $ zipWith (*) r v) m
>              else error $ "Matrix has rows of length " ++ show lrs ++
>                           " but vector is of length " ++ show l

For example, we can ask ghci to tell us the type

    [ghci]
    :t matTimesVecNoSig

> matTimesVec :: Num a => Matrix a -> Vector a -> Vector a
> matTimesVec m v = result
>   where
>     lrs = map length m
>     l   = length v
>     result = if all (== l) lrs
>              then map (\r -> sum $ zipWith (*) r v) m
>              else error $ "Matrix has rows of length " ++ show lrs ++
>                           " but vector is of length " ++ show l
