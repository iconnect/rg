{-# LANGUAGE OverloadedStrings          #-}

module Main (main) where

import           Data.Rg
import           Test.Tasty
import           Test.Tasty.HUnit


main :: IO ()
main = defaultMain $
    testGroup "rg" $
    [ testCase "sizeRg"         $ 10        @=? sizeRg tst
    , testCase "allListRg"      $ [10,9..1] @=? map extractRange (allListRg tst)
    , testCase "predRg"         $ Nothing   @=? (extractRange <$> predRg tst)
    , testCase "predRg . maxRg" $ Just 2    @=? (extractRange <$> predRg (maxRg tst))
    , testCase "succRg"         $ Just 9    @=? (extractRange <$> succRg tst)
    , testCase "extractRange"   $ 10        @=? extractRange tst
    , testCase "minRg"          $ 10        @=? extractRange (minRg tst)
    , testCase "maxRg"          $ 1         @=? extractRange (maxRg tst)
    , testCase "extractRange"   $ 1         @=? extractRange (maxRg tst)
    , testCase "fromRg"         $ 0         @=? fromRg tst
    , testCase "minRg"          $ 0         @=? fromRg (minRg tst)
    , testCase "maxRg"          $ 9         @=? fromRg (maxRg tst)
    , testCase "toRg"           $ Just 5    @=? (fromRg <$> toRg tst 5)
    ]
  where
    tst :: Range Int
    tst = newStartOfRangeFromList [10,9..1]
