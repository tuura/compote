{-
    Author: Andrey Mokhov, Newcastle University
    Date: 24 December 2012
    Contact: andrey.mokhov@ncl.ac.uk
    Description: Haskell interface to BDD implementation in C++.
-}

{-# LANGUAGE TypeFamilies #-}

module BDD (Node, setCacheSize, performGC) where

import Control.Monad
import Prelude hiding (and, or, not)
import System.IO.Unsafe
import Foreign hiding (xor, unsafePerformIO)
import Foreign.C.Types
import Predicate
import Data.Algebra.Boolean

data NodeStruct
type NodeID = Ptr NodeStruct
newtype Node v = Node (ForeignPtr NodeStruct)

getID :: Node v -> NodeID
getID (Node node) = unsafePerformIO $ withForeignPtr node return

getNode :: NodeID -> IO (Node v)
getNode id = fmap Node $ newForeignPtr dereferenceID id

foreign import ccall "setCacheSize"
    setCacheSize :: CSize -> IO ()

foreign import ccall "performGC"
    performGC :: IO ()

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

ite' :: Node v -> Node v -> Node v -> Node v
ite' (Node f) (Node g) (Node h) = unsafePerformIO $ 
                                                    withForeignPtr f $ \fid ->
                                                    withForeignPtr g $ \gid ->
                                                    withForeignPtr h $ \hid -> getNode =<< iteID fid gid hid

iteTrue' :: Node v -> Node v -> Node v -> Bool
iteTrue' (Node f) (Node g) (Node h) = (1 ==) $ unsafePerformIO $ 
                                                                withForeignPtr f $ \fid ->
                                                                withForeignPtr g $ \gid ->
                                                                withForeignPtr h $ \hid -> iteTrueID fid gid hid

instance Boolean (Node v) where
    true         = unsafePerformIO $ getNode =<< oneID
    false        = unsafePerformIO $ getNode =<< zeroID
    not (Node x) = unsafePerformIO $ withForeignPtr x $ getNode <=< notID
    x && y       = ite' x y       false
    x || y       = ite' x true    y
    xor x y      = ite' x (not y) y
    x --> y      = ite' x y       true
    x <--> y     = ite' x y       true

instance Enum v => Predicate (Node v) where
    type Variable (Node v) = v
    variable = unsafePerformIO . (getNode <=< variableID) . fromEnum
    ite      = ite'
    iteTrue  = iteTrue'

instance Eq (Node v) where
    p == q = (getID p) == (getID q) 

instance Ord (Node v) where
    p <= q = iteTrue' p q true
