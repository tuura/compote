{-
	Author: Andrey Mokhov, Newcastle University
	Date: 1 December 2012
	Contact: andrey.mokhov@ncl.ac.uk
	Description: Basic data type for algebra of switching networks.
-}

module SN (SN (..), foldSN) where

data SN a b = Epsilon
			| Vertex a
			| Overlay (SN a b) (SN a b)
			| Connection (SN a b) (SN a b)
			| Condition b (SN a b)



instance (Show a, Show b) => Show (SN a b) where
	showsPrec _ Epsilon          = showChar 'ε'
	showsPrec _ (Vertex v)       = shows v
	showsPrec d (Overlay p q)    = showParen (d > 0) $ shows p . showChar ' ' . shows q
	showsPrec d (Connection p q) = showParen (d > 1) $ showsPrec 1 p . showString " —— " . showsPrec 1 q
	showsPrec d (Condition x p)  = showChar '[' . shows x . showChar ']' . showsPrec 2 p
