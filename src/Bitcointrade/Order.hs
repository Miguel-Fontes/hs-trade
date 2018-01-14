{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Bitcointrade.Order where

import Network.HTTP.Conduit
import Data.Aeson
import Control.Monad
import GHC.Generics

data Message = Message { message :: String
                       , responseData :: Entries 
                       } deriving (Show)

instance FromJSON Message where
    parseJSON (Object v) = 
        Message <$> v .:? "message" .!= "null"
                <*> v .: "data"
    
    parseJSON _ = mzero

data Entries = Bids { bids :: [Order]
                    } 
             | Asks { asks :: [Order]
                    } deriving (Show, Generic, ToJSON, FromJSON) 

data Order = Order { unit_price :: Float
                   , code :: String
                   , amount :: Float 
                   } deriving (Show, Generic, ToJSON, FromJSON)

getOrders :: IO (Either String Entries)
getOrders = do
    response <- simpleHttp "https://api.bitcointrade.com.br/v1/public/BTC/orders"

    print response

    return $ case (eitherDecode response :: Either String Message) of
        Left err -> Left err
        Right (Message _ orders) -> Right orders