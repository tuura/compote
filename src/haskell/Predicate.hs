{-
	Author: Andrey Mokhov, Newcastle University
	Date: 25 December 2012
	Contact: andrey.mokhov@{ncl.ac.uk, gmail.com}
	Description: Predicate class.
-}

{-# LANGUAGE TypeFamilies #-}

module Predicate (Predicate(..), Boolean(..)) where

import Prelude hiding ((&&), (||), not)
import Data.Algebra.Boolean

class Boolean a => Predicate a where
	type Variable a
	variable :: Variable a -> a

	ite      :: a -> a -> a -> a
	iteTrue  :: Eq a => a -> a -> a -> Bool

	ite     f g h = f && g || (not f) && h
	iteTrue f g h = (ite f g h) == true
