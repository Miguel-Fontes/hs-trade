module Main where

import           Bitcointrade.Order
import           Bitcointrade.Ticker

import           Text.Printf
import           System.Exit
import           System.Environment

main :: IO ()
main = do
  cmdArgs <- getArgs >>= \args -> return $ case args of
    [] -> ["no option"]
    xs -> xs

  case lookup (head cmdArgs) availableOptions of
    Just option -> option
    Nothing -> putStrLn (getErrorMessage)

availableOptions :: [(String, IO ())]
availableOptions = [("ticker", getTickerAction)
                   ,("ofertas", getOrdersAction)
                   ]

getOptionsNames :: [(String, IO ())] -> [String]
getOptionsNames (x:xs) = fst x : getOptionsNames xs
getOptionsNames [] = []

getErrorMessage :: String
getErrorMessage = "Opção inválida selecionada! Por favor, execute a aplicação " 
              ++ "selecionando uma das opções válidas: " 
              ++ (show . getOptionsNames $ availableOptions)
              ++ "\nEx: hs-trade ofertas"

getTickerAction :: IO ()  
getTickerAction = do
  tickerResponse <- getTicker
  case tickerResponse of
    Left err -> print err
    Right t  -> putStrLn ("> Estatísticas das ultimas 24h (Ticker)\n" ++ (show t))

getOrdersAction :: IO ()
getOrdersAction = do
  ordersResponse <- getOrders
  case ordersResponse of
    Left err -> print err
    Right orders -> do
      let totalBids = totalBidsValue orders
          totalAsks = totalAsksValue orders

      putStrLn "> Estatísticas do Book de Ordens"
      printf "%-20s : %15d\n"      "Qtd vendas"    (numberOfAsks orders)
      printf "%-20s : R$ %12.2f\n" "Média val vendas" (averageAskValue orders)
      printf "%-20s : R$ %12.2f\n" "Valor tot vendas"  totalAsks
      putStrLn ("")

      printf "%-20s : %15d\n"      "Qtd compras"    (numberOfBids orders)
      printf "%-20s : R$ %12.2f\n" "Média val compras" (averageBidValue orders)
      printf "%-20s : R$ %12.2f\n" "Valor tot compras"  totalBids
      putStrLn ("")

      printf "%-20s : R$ %12.2f\n" "Tot vendas-compras" (totalAsks - totalBids)
      putStrLn ("")

      printf "> Distribuição de ordens de venda (intervalo=10000; x = no vendas até x-10000)\n%s" (prettify $ generateAsksOrderGroups 0 10000 orders)
      putStrLn ("")

      printf "> Distribuição de ordens de compra (intervalo=10000; x = no compras até x-10000)\n%s" (prettify $ generateBidsOrderGroups 0 10000 orders)
      putStrLn ("")
      
