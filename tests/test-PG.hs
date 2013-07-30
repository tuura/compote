{-
	Author: Andrey Mokhov, Newcastle University
	Date: 10 January 2013
	Contact: andrey.mokhov@{ncl.ac.uk, gmail.com}
	Description: Testing PGs.
-}

import PG
import Elements
import Predicate
import BDD
import Prelude hiding (not, (&&), (||))

x = variable 0 :: Node Int
y = variable 1 :: Node Int
z = variable 2 :: Node Int

x' = not x
y' = not y
z' = not z

a = Vertex "a" :: PG String (Node Int)
b = Vertex "b"
c = Vertex "c"

test :: Bool -> IO ()
test True = putStrLn "OK"
test False = putStrLn "FAIL"

main = do
	   test $ a == a
	   test $ a /= b
	   test $ x ? a ˽ x' ? a == a
	   test $ a ~> b ~> c == a ~> b ˽ a ~> c ˽ b ~> c
	   test $ a ~> b ~> c /= a ~> b ˽ b ~> c
	   test $ x ? (a ˽ b) == x ? a ˽ x ? b
	   test $ (x && z || y && z') == (x && y || x && z || y && z')
	   test $ (x && z || y && z') ? a == (x && y || x && z || y && z') ? a
	   test $ x ? z ? a ˽ y ? z' ? a == x ? y ? a ˽ x ? z ? a ˽ y ? z' ? a
	   test $ (x && x') == false
	   test $ (x ? a) ~> (x' ? b) == (x ? a) ˽ (x' ? b)
	   test $ (true ? a) ~> (false ? b) == a
	   test $ x ? (a ~> b) ˽ x' ? (a ~> c) == a ~> (x ? b ˽ x' ? c)
	   test $ x ? a ~> y ? b == x ? a ˽ y ? b ˽ (x && y) ? (a ~> b)
	   test $ x ? x' ? a == ε
	   test $ x ? ε == (ε :: PG String (Node Int))
	   test $ x ? (ε ˽ a) == x ? a
	   putStrLn "Done"
