module Main where

import           Bitcointrade.Order
import           Bitcointrade.Ticker

import           Text.Printf
import           System.Exit
import           System.Environment

main :: IO ()
main = do
  cmdArgs <- getArgs

  case head cmdArgs of 
    "ticker" -> getTickerData
    "orders" -> getOrdersData
    _        -> print ("No option selected!\n")

getTickerData :: IO ()  
getTickerData = do
  tickerResponse <- getTicker
  case tickerResponse of
    Left err -> print err
    Right t  -> print t

getOrdersData :: IO ()
getOrdersData = do
  ordersResponse <- getOrders
  case ordersResponse of
    Left err -> print err
    Right orders -> do
      let totalBids = totalBidsValue orders
          totalAsks = totalAsksValue orders

      printf "%-20s : %15d\n"      "Qtd vendas"    (numberOfAsks orders)
      printf "%-20s : R$ %12.2f\n" "Média val vendas" (averageAskValue orders)
      printf "%-20s : R$ %12.2f\n" "Valor tot vendas"  totalAsks
      putStrLn ("")

      printf "%-20s : %15d\n"      "Qtd compras"    (numberOfBids orders)
      printf "%-20s : R$ %12.2f\n" "Média val compras" (averageBidValue orders)
      printf "%-20s : R$ %12.2f\n" "Valor tot compras"  totalBids
      putStrLn ("")

      printf "%-20s : R$ %12.2f\n" "Tot vendas-compras" (totalAsks - totalBids)
      
