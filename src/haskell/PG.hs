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
import PS
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

class MapPG m where
	mapPG :: (Epsilon m, Vertex m, Overlay m, Sequence m, Condition m, Alphabet m ~ a, Parameter m ~ b) => PG a b -> m
	mapPG Epsilon         = ε
	mapPG (Vertex e)      = vertex e
	mapPG (Overlay p q)   = mapPG p ˽ mapPG q
	mapPG (Sequence p q)  = mapPG p ~> mapPG q
	mapPG (Condition x p) = x ? mapPG p

-- PG normal form

newtype PGNF a b = PGNF ([(a, b)], [((a, a), b)]) deriving Eq

instance Epsilon (PGNF a b) where ε = PGNF (ε, ε)

instance (Boolean b) => Vertex (PGNF a b) where
	type Alphabet (PGNF a b) = a
	vertex v = PGNF (vertex v, ε)

instance (Ord a, Boolean b) => Overlay (PGNF a b) where
	PGNF (p1, p2) ˽ PGNF (q1, q2) = PGNF (p1 ˽ q1, p2 ˽ q2)

instance (Ord a, Boolean b) => Sequence (PGNF a b) where
	PGNF (p1, p2) ~> PGNF (q1, q2) = PGNF (p1 ˽ q1, [ ((from, to), x && y) | (from, x) <- p1, (to, y) <- q1 ] ˽ (p2 ˽ q2))

instance (Eq b, Boolean b) => Condition (PGNF a b) where
	type Parameter (PGNF a b) = b
	x ? PGNF (p, q) = PGNF (x ? p, x ? q)

instance MapPG (PGNF a b)

instance (Ord a, Eq b, Boolean b) => NormalForm (PG a b) where
	type NF (PG a b) = PGNF a b
	toNF                 = mapPG
	fromNF (PGNF (p, q)) = foldr (˽) arcs $ map (\(v, x) -> x ? vertex v) p
						   where
								arcs = foldr (˽) ε $ map (\((u, v), x) -> x ? (vertex u ~> vertex v)) q

instance (Show a, Show b) => Show (PG a b) where
	showsPrec _ Epsilon         = showChar 'ε'
	showsPrec _ (Vertex v)      = shows v
	showsPrec d (Overlay p q)   = showParen (d > 0) $ shows p . showChar ' ' . shows q
	showsPrec d (Sequence p q)  = showParen (d > 1) $ showsPrec 1 p . showString " —→ " . showsPrec 1 q
	showsPrec d (Condition x p) = showChar '[' . shows x . showChar ']' . showsPrec 2 p

instance (Show a, Show b) => Show (PGNF a b) where
	showsPrec _ (PGNF (p, q)) = shows p . showChar ' ' . shows q

instance (Ord a, Eq b, Boolean b) => Eq (PG a b) where
	p == q = toNF p == toNF q
