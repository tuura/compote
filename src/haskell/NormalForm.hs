{-
	Author: Andrey Mokhov, Newcastle University
	Date: 7 January 2013
	Contact: andrey.mokhov@{ncl.ac.uk, gmail.com}
	Description: Type class for Normal Forms.
-}

{-# LANGUAGE TypeFamilies #-}

module NormalForm where

class NormalForm a where
	type NF a
	toNF   :: a -> NF a
	fromNF :: NF a -> a
