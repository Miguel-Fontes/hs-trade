module Main where

import Bitcointrade.Ticker

import System.Exit

main :: IO ()
main = do
  tickerResponse <- getTicker

  case tickerResponse of
    Left err -> print err
    Right t -> print t

  print "okay"
