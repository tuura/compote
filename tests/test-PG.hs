{-
	Author: Andrey Mokhov, Newcastle University
	Date: 10 January 2013
	Contact: andrey.mokhov@{ncl.ac.uk, gmail.com}
	Description: Testing PGs.
-}

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck (testProperty)

import Test.QuickCheck
import Test.HUnit

main = defaultMain tests

tests = [
		testGroup "Equality tests"
			[
			testCase "Epsilon" test_sort7
			],
	    testGroup "Sorting Group 2" [
	                testProperty "sort4" prop_sort4,
	                testProperty "sort5" prop_sort5,
	                testProperty "sort6" prop_sort6,
	                testCase "sort7" test_sort7,
	                testCase "sort8" test_sort8
            ]
    ]
