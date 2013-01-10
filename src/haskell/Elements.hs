{-
	Author: Andrey Mokhov, Newcastle University
	Date: 7 January 2013
	Contact: andrey.mokhov@{ncl.ac.uk, gmail.com}
	Description: Type classes of basic algebraic elements and operations.
-}

{-# LANGUAGE TypeFamilies, FlexibleInstances #-}

module Elements (
				Epsilon, epsilon, ε,
				Vertex, vertex, Alphabet,
				Overlay, overlay, (˽),
				Sequence, Elements.sequence, (~>),
				Connection, connection, (~~),
				Condition, condition, (?), Parameter
				) where

import Prelude hiding (sequence, (||), (&&))
import Data.Algebra.Boolean

class Epsilon a where
	epsilon, ε :: a

	ε       = epsilon
	epsilon = ε

class Vertex a where
	type Alphabet a
	vertex :: Alphabet a -> a

class Overlay a where
	overlay, (˽) :: a -> a -> a

	(˽)     = overlay
	overlay = (˽)

class Sequence a where
	sequence, (~>) :: a -> a -> a

	(~>)     = sequence
	sequence = (~>)

class Condition a where
	type Parameter a
	condition, (?) :: Parameter a -> a -> a

	(?)       = condition
	condition = (?)

class Connection a where
	connection, (~~) :: a -> a -> a

	(~~)       = connection
	connection = (~~)

infixl 6 ˽
infixl 7 ~>
infixl 7 ~~
infixr 8 ?

-- Instances useful for normal forms

instance Epsilon [(a, b)] where ε = []

instance Boolean b => Vertex [(a, b)] where
	type Alphabet [(a, b)] = a
	vertex v = [(v, true)]

instance (Ord a, Boolean b) => Overlay [(a, b)] where
	[] ˽ rest                 = rest
	rest ˽ []                 = rest
	((u, x):us) ˽ ((v, y):vs) | u < v     = (u, x) : us ˽ ((v, y):vs)
	                          | u > v     = (v, y) : ((u, x):us) ˽ vs
	                          | otherwise = (u, x || y) : us ˽ vs

instance (Eq b, Boolean b) => Condition [(a, b)] where
	type Parameter [(a, b)] = b
	x ? p = filter (\(_, x) -> (x /= false)) $ map (\(v, y) -> (v, x && y)) p

