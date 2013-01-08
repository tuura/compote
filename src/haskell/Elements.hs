{-
	Author: Andrey Mokhov, Newcastle University
	Date: 7 January 2013
	Contact: andrey.mokhov@{ncl.ac.uk, gmail.com}
	Description: Type classes of basic algebraic elements and operations.
-}

{-# LANGUAGE TypeFamilies #-}

module Elements (
				Epsilon, epsilon, ε,
				Vertex, vertex, Alphabet,
				Overlay, overlay, (˽),
				Sequence, Elements.sequence, (~>),
				Connection, connection, (~~),
				Condition, condition, (?), Parameter
				) where

import qualified Prelude

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
infixr 8 ?
