module Main where

import           Bitcointrade.Order
import           Bitcointrade.Ticker

import           System.Exit

main :: IO ()
main = do
  tickerResponse <- getTicker
  case tickerResponse of
    Left err -> print err
    Right t  -> print t
  
  ordersResponse <- getOrders
  case ordersResponse of
    Left err -> print err
    Right orders -> do
      print ("Number of asks: " ++ (show . length . asks $ orders))
      print ("Number of bids: " ++ (show . length . bids $ orders))
  
