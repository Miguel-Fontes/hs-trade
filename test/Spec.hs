module Main where

import qualified Format.StringTest as S
import qualified Format.NumericTest as N
import qualified Bitcointrade.OrderTest as O
import qualified Bitcointrade.TickerTest as T


main = do
    putStr("\n> Format.StringTest")
    S.test

    putStr("\n> Format.NumericTest")
    N.test

    putStr("\n> Bitcointrade.OrderTest")
    O.test

    putStr("\n> Format.TickerTest")
    T.test
