{-
	Author: Andrey Mokhov, Newcastle University
	Date: 10 January 2013
	Contact: andrey.mokhov@{ncl.ac.uk, gmail.com}
	Description: Parameterised Set data type.
-}

{-# LANGUAGE TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}

module PS (PS) where

import Prelude hiding ((||), (&&))
import Elements
import NormalForm
import Data.Algebra.Boolean

data PS a b = Epsilon
			| Vertex a
			| Overlay (PS a b) (PS a b)
			| Condition b (PS a b)

instance  Epsilon (PS a b) where ε    = Epsilon
instance  Overlay (PS a b) where (˽)  = Overlay

instance Vertex (PS a b) where
	type Alphabet (PS a b) = a
	vertex = Vertex

instance Condition (PS a b) where
	type Parameter (PS a b) = b
	(?) = Condition

class MapPS m where
	mapPS :: (Epsilon m, Vertex m, Overlay m, Condition m, Alphabet m ~ a, Parameter m ~ b) => PS a b -> m
	mapPS Epsilon         = ε
	mapPS (Vertex e)      = vertex e
	mapPS (Overlay p q)   = mapPS p ˽ mapPS q
	mapPS (Condition x p) = x ? mapPS p

instance MapPS [(a, b)]

instance (Ord a, Eq b, Boolean b) => NormalForm (PS a b) where
	type NF (PS a b) = [(a, b)]
	toNF   = mapPS
	fromNF = foldr (˽) ε . map (\(v, x) -> x ? vertex v)

instance (Show a, Show b) => Show (PS a b) where
	showsPrec _ Epsilon         = showChar 'ε'
	showsPrec _ (Vertex v)      = shows v
	showsPrec d (Overlay p q)   = showParen (d > 0) $ shows p . showChar ' ' . shows q
	showsPrec d (Condition x p) = showChar '[' . shows x . showChar ']' . showsPrec 2 p

instance (Ord a, Eq b, Boolean b) => Eq (PS a b) where
	p == q = toNF p == toNF q
