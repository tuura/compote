{-
	Author: Andrey Mokhov, Newcastle University
	Date: 24 December 2012
	Contact: andrey.mokhov@{ncl.ac.uk, gmail.com}
	Description: Haskell interface to BDD implementation in C++.
-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module BDD (Node, setCacheSize, runGC, clear) where

import Control.Monad
import Prelude hiding (and, or, not)
import System.IO.Unsafe
import Foreign hiding (xor, unsafePerformIO)
import Foreign.C.Types
import Predicate
import Data.Algebra.Boolean
import Control.Monad.Cont
import Control.Applicative

data NodeStruct
type NodeID = Ptr NodeStruct
newtype Node v = Node (ForeignPtr NodeStruct)

extractNode :: Node v -> ContT r IO NodeID
extractNode (Node ptr) = ContT (withForeignPtr ptr)

class Convertible a b | a -> b where
  convertC :: (forall r. ContT r IO a) -> b
  convert :: a -> b
  convert a = convertC (return a)

instance Convertible (IO Int) Int where
  convertC raw = unsafePerformIO $ runContT raw id

instance Convertible (IO NodeID) (Node v) where
  convertC raw = unsafePerformIO $ runContT raw (>>=getNode)

instance Convertible raw cooked => Convertible (NodeID -> raw) (Node v -> cooked) where
  convertC f n = convertC $ f <*> extractNode n

getID :: Node v -> NodeID
getID (Node node) = unsafePerformIO $ withForeignPtr node return

getNode :: NodeID -> IO (Node v)
getNode id = fmap Node $ newForeignPtr dereferenceID id

foreign import ccall "setCacheSize"
	setCacheSize :: CSize -> IO ()

foreign import ccall "performGC"
	runGC :: IO ()

foreign import ccall "clear"
	clear :: IO ()

foreign import ccall "one"
	oneID :: IO NodeID

foreign import ccall "zero"
	zeroID :: IO NodeID

foreign import ccall "notGate"
	notID :: NodeID -> IO NodeID

foreign import ccall "ite"
	iteID :: NodeID -> NodeID -> NodeID -> IO NodeID

foreign import ccall "&dereference"
	dereferenceID :: FunPtr (NodeID -> IO ())

foreign import ccall "variable"
	variableID :: Int -> IO NodeID

foreign import ccall "iteTrue"
	iteTrueID :: NodeID -> NodeID -> NodeID -> IO Int

ite' = convert iteID

iteTrue' :: Node v -> Node v -> Node v -> Bool
iteTrue' f g h = 1 == convert iteTrueID f g h

instance Boolean (Node v) where
	true         = convert oneID
	false        = convert zeroID
	not          = convert notID
	x && y       = ite' x y       false
	x || y       = ite' x true    y
	xor x y      = ite' x (not y) y
	x --> y      = ite' x y       true
	x <--> y     = ite' x y       true

instance Enum v => Predicate (Node v) where
	type Variable (Node v) = v
	variable = convert . variableID . fromEnum
	ite      = ite'
	iteTrue  = iteTrue'

instance Eq (Node v) where
	p == q = (getID p) == (getID q)

instance Ord (Node v) where
	p <= q = iteTrue' p q true
