{-# LANGUAGE OverloadedStrings #-}

module Bitcointrade.Order (
    Entries,
    getOrders,
    numberOfAsks,
    numberOfBids,
    averageAskValue,
    averageBidValue,
    totalAsksValue,
    totalBidsValue,
    generateAsksOrderGroups,
    generateBidsOrderGroups,
    prettify
) where

import           Control.Monad
import           Data.Aeson
import           GHC.Generics
import           Network.HTTP.Conduit

data Message = Message
  { message      :: String
  , responseData :: Entries
  } deriving (Show)

instance FromJSON Message where
  parseJSON (Object v) = 
    Message <$> v .:? "message" .!= "null" 
            <*> v .: "data"
  
  parseJSON _ = mzero

data Entries = Entries
  { bids :: [Order]
  , asks :: [Order]
  } deriving (Show)

instance FromJSON Entries where
  parseJSON (Object v) = Entries <$> v .: "bids" <*> v .: "asks"

data Order = Order
  { unit_price :: Double
  , code       :: String
  , amount     :: Double
  } deriving (Show)

instance FromJSON Order where
  parseJSON (Object v) =
    Order <$> v .: "unit_price" <*> 
    v .: "code" <*> 
    v .: "amount"
  
  parseJSON _ = mzero

getOrders :: IO (Either String Entries)
getOrders = do
  response <- simpleHttp "https://api.bitcointrade.com.br/v1/public/BTC/orders"
  
  return $
    case (eitherDecode response :: Either String Message) of
      Left err                 -> Left err
      Right (Message _ orders) -> Right orders

numberOfAsks :: Entries -> Int
numberOfAsks = length . asks

numberOfBids :: Entries -> Int
numberOfBids = length . bids

averageAskValue :: Entries -> Double
averageAskValue = calculateAverageTradesValue . asks

averageBidValue :: Entries -> Double
averageBidValue = calculateAverageTradesValue . bids

totalValue :: Order -> Double
totalValue (Order unitPrice _ amount) = unitPrice * amount

calculateAverageTradesValue :: [Order] -> Double
calculateAverageTradesValue orders = 
    let stats = foldl step (0, 0) orders
    in  calculateAverage stats
      where step (sum, total) order = (sum + totalValue order, total + 1)
            calculateAverage (sum, total) = sum / total

totalAsksValue :: Entries -> Double
totalAsksValue = calculateTotalValue . asks

totalBidsValue :: Entries -> Double
totalBidsValue = calculateTotalValue . bids

calculateTotalValue :: [Order] -> Double
calculateTotalValue = foldl step 0
    where step sum order = sum + totalValue order

type OrderGroup = (Double, Int)

generateAsksOrderGroups :: Double -> Double -> Entries -> [OrderGroup]
generateAsksOrderGroups initial step = (generateOrderGroups initial step) . asks

generateBidsOrderGroups :: Double -> Double -> Entries -> [OrderGroup]
generateBidsOrderGroups initial step = (generateOrderGroups initial step) . bids


generateOrderGroups :: Double -> Double -> [Order] -> [OrderGroup]
generateOrderGroups _ _ [] = []
generateOrderGroups initial step orders = let (cOrders, orderGroup) = group (orders, (initial, 0))
                                           in orderGroup : generateOrderGroups (initial + step) step cOrders
  where group ([], groups) = ([], groups)
        group ((v:vs), (limit, n)) 
            | unit_price v <= limit = group (vs, (limit, n + 1))
            | otherwise = (v:vs, (limit, n))

prettify :: [OrderGroup] -> String
prettify = (foldr step "")
    where step (group, count) acc
              | count > 0 = show group ++ ": " ++ show count ++ "\n" ++ acc
              | otherwise = acc
