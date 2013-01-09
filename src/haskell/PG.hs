{-
	Author: Andrey Mokhov, Newcastle University
	Date: 20 July 2012
	Contact: andrey.mokhov@{ncl.ac.uk, gmail.com}
	Description: Parameterised Graphs (PGs).
-}

{-# LANGUAGE TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}

module PG (PG (..)) where

import Elements
import NormalForm
import Data.Algebra.Boolean
import Prelude hiding ((&&), (||))

data PG a b = Epsilon
			| Vertex a
			| Overlay (PG a b) (PG a b)
			| Sequence (PG a b) (PG a b)
			| Condition b (PG a b)

instance  Epsilon (PG a b) where ε    = Epsilon
instance  Overlay (PG a b) where (˽)  = Overlay
instance Sequence (PG a b) where (~>) = Sequence

instance Vertex (PG a b) where
	type Alphabet (PG a b) = a
	vertex = Vertex

instance Condition (PG a b) where
	type Parameter (PG a b) = b
	(?) = Condition

--class MapPG a where
--	type ResultAlphabet a
--	type ResultParameter b
--	mapPG :: (Epsilon b, Vertex b, Overlay b, Sequence b, Condition b, ResultAlphabet a ~ Alphabet b, ResultParameter a ~ Parameter b) => a -> b

--instance MapPG (PG a b) where
--	type ResultAlphabet (PG a b) = a
--	type ResultParameter (PG a b) = b
--	mapPG Epsilon         = ε
--	mapPG (Vertex v)      = vertex v
--	mapPG (Overlay p q)   = p ˽ q
--	mapPG (Sequence p q)  = p ~> q
--	mapPG (Condition x p) = x ? p

--mapPG :: (Epsilon r, Vertex r, Overlay r, Sequence r, Condition r) => PG (Alphabet r) (Parameter r) -> r
--mapPG Epsilon         = ε
--mapPG (Vertex v)      = vertex v
--mapPG (Overlay p q)   = p ˽ q
--mapPG (Sequence p q)  = p ~> q
--mapPG (Condition x p) = x ? p

-- PG normal form

--instance Epsilon (PGNF a b) where ε = ([], [])

--instance Boolean b => Vertex (PGNF a b) where
--	type Alphabet (PGNF a b) = a
--	vertex v = ([(v, true)], [])

--instance (Ord a, Boolean b) => Overlay (PGNF a b) where
--	(pv, pa) ˽ (qv, qa) = (pv \./ qv, pa \./ qa)

--instance (Ord a, Boolean b) => Sequence (PGNF a b) where
--	(pv, pa) ~> (qv, qa) = (pv \./ qv, [ ((from, to), x && y) | (from, x) <- pv, (to, y) <- qv ] \./ (pa \./ qa))

--instance Boolean b => Condition (PGNF a b) where
--	type Parameter (PGNF a b) = b
--	x ? (v, a) = (map (\(t, f) -> (t, x && f)) v, map (\(t, f) -> (t, x && f)) a)

type VertexLiteral a b = (a, b)
type ArcLiteral a b = ((a, a), b)
type PGNF a b = ([VertexLiteral a b], [ArcLiteral a b])

instance (Ord a, Boolean b) => NormalForm (PG a b) where
	type NF (PG a b) = PGNF a b

	toNF Epsilon         = ε
	toNF (Vertex v)      = vertex v
	toNF (Overlay p q)   = toNF p ˽  toNF q
	toNF (Sequence p q)  = toNF p ~> toNF q
	toNF (Condition x p) = x ? toNF p



instance (Show a, Show b) => Show (PG a b) where
	showsPrec _ Epsilon         = showChar 'ε'
	showsPrec _ (Vertex v)      = shows v
	showsPrec d (Overlay p q)   = showParen (d > 0) $ shows p . showChar ' ' . shows q
	showsPrec d (Sequence p q)  = showParen (d > 1) $ showsPrec 1 p . showString " —→ " . showsPrec 1 q
	showsPrec d (Condition x p) = showChar '[' . shows x . showChar ']' . showsPrec 2 p

instance (Ord a, Boolean b, Eq b) => Eq (PG a b) where
	p == q = toNF p == toNF q
