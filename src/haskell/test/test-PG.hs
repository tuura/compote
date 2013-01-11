{-
	Author: Andrey Mokhov, Newcastle University
	Date: 10 January 2013
	Contact: andrey.mokhov@{ncl.ac.uk, gmail.com}
	Description: Testing PGs.
-}

test :: Bool -> IO ()
test True = putStrLn "OK"
test False = putStrLn "FAIL"

