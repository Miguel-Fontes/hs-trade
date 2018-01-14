{-# LANGUAGE OverloadedStrings #-}
module Bitcointrade.Ticker where

import Network.HTTP.Conduit
import Data.Aeson
import Control.Monad

data Message = Message { message :: String
                       , ticker :: Ticker 
                       } deriving (Show)

instance FromJSON Message where
  parseJSON (Object v) =  
    Message <$> v .:? "message" .!= "null"
            <*> v .: "data"

  parseJSON _ = mzero

data Ticker = Ticker { high :: Float
                     , low :: Float
                     , volume :: Float
                     , trades_quantity :: Integer
                     , last :: Float
                     , sell :: Float
                     , buy :: Float
                     , date :: String
                     } deriving (Show)

instance FromJSON Ticker where
  parseJSON (Object v) =
    Ticker <$> v .: "high"
    <*> v .: "low"
    <*> v .: "volume"
    <*> v .: "trades_quantity"
    <*> v .: "last"
    <*> v .: "sell"
    <*> v .: "buy"
    <*> v .: "date"
  
  parseJSON _ = mzero

getTicker :: IO (Either String Ticker)
getTicker = do 
  response <- simpleHttp "https://api.bitcointrade.com.br/v1/public/BTC/ticker"

  return $ case (eitherDecode response :: Either String Message) of
    Left err -> Left err
    Right (Message _ ticker) -> Right ticker