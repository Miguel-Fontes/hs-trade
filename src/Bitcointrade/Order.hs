{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Bitcointrade.Order where

import           Control.Monad
import           Data.Aeson
import           Data.Foldable        (asum)
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
  } deriving (Show, Generic)

instance FromJSON Entries where
  parseJSON (Object v) = Entries <$> v .: "bids" <*> v .: "asks"

data Order = Order
  { unit_price :: Float
  , code       :: String
  , amount     :: Float
  } deriving (Show, Generic)

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
