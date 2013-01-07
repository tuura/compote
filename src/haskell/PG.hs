{-
	Author: Andrey Mokhov, Newcastle University
	Date: 20 July 2012
	Contact: andrey.mokhov@{ncl.ac.uk, gmail.com}
	Description: Parameterised Graphs (PGs).
-}

{-# LANGUAGE TypeFamilies #-}

module PG (PG (..), ε, (˽), (~>), (?)) where

import NormalForm
import Data.Algebra.Boolean
import Prelude hiding ((&&), (||))

data PG a b = Epsilon
			| Vertex a
			| Overlay (PG a b) (PG a b)
			| Sequence (PG a b) (PG a b)
			| Condition b (PG a b)

ε    = Epsilon
(˽)  = Overlay
(~>) = Sequence
(?)  = Condition

infixl 6 ˽
infixl 7 ~>
infixr 8 ?

instance (Show a, Show b) => Show (PG a b) where
	showsPrec _ Epsilon         = showChar 'ε'
	showsPrec _ (Vertex v)      = shows v
	showsPrec d (Overlay p q)   = showParen (d > 0) $ shows p . showChar ' ' . shows q
	showsPrec d (Sequence p q)  = showParen (d > 1) $ showsPrec 1 p . showString " —→ " . showsPrec 1 q
	showsPrec d (Condition x p) = showChar '[' . shows x . showChar ']' . showsPrec 2 p

instance (Ord a, Boolean b, Eq b) => Eq (PG a b) where
	p == q = toNF p == toNF q

instance (Ord a, Boolean b, Eq b) => Ord (PG a b) where
	p <= q = toNF (p ˽ q) == toNF q

type VertexLiteral a b = (a, b)
type ArcLiteral a b = ((a, a), b)

instance (Ord a, Boolean b) => NormalForm (PG a b) where
	type NF (PG a b) = ([VertexLiteral a b], [ArcLiteral a b])

	toNF Epsilon         = ([], [])
	toNF (Vertex v)      = ([(v, true)], [])
	toNF (Overlay p q)   = (pv \./ qv, pa \./ qa)
							where
								(pv, pa) = toNF p
								(qv, qa) = toNF q

	toNF (Sequence p q)  = (pv \./ qv, pqa \./ (pa \./ qa))
							where
								(pv, pa) = toNF p
								(qv, qa) = toNF q
								pqa      = [ ((u, v), p && q) | (u, p) <- pv, (v, q) <- qv ]

	toNF (Condition x p) = (map f v, map f a)
							where
								(v, a) = toNF p
								f (t, g) = (t, x && g)

(\./) :: (Ord a, Boolean b) => [(a, b)] -> [(a, b)] -> [(a, b)]
[] \./ rest                 = rest
rest \./ []                 = rest
((u, p):us) \./ ((v, q):vs) | u < v     = (u, p) : us \./ ((v, q):vs)
							| u > v     = (v, q) : ((u, p):us) \./ vs
							| otherwise = (u, p || q) : us \./ vs
