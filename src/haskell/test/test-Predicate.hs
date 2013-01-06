{-
    Author: Andrey Mokhov, Newcastle University
    Date: 24 December 2012
    Contact: andrey.mokhov@ncl.ac.uk
    Description: Testing Haskell interface to BDDs.
-}

{-# LANGUAGE NoImplicitPrelude #-}

import NumericPrelude
import Predicate
import BDD
--import Prelude hiding (not, (&&), (||))

x = variable 0
y = variable 1
z = (variable 2) :: Node Int

x' = not x
y' = not y
z' = not z

test :: Bool -> IO ()
test True = putStrLn "OK"
test False = putStrLn "FAIL"

main = do
       test $ (true :: Node Int) /= false
       test $ x == not x'
       test $ x' == ite x false true
       test $ (x && y) == (not (x' || y'))
       test $ (x && z || y && z') == (x && y || x && z || y && z')
       setCacheSize 65536
       test $ ((x `xor` y) `xor` (x `xor` y')) == true
       test $ (x && y) < x
       test $ x' < (x' || y')
       test $ iteTrue (x && x') false true
       putStrLn "GC..."
       performGC
       putStrLn "Done"


