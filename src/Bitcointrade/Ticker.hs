{-# LANGUAGE OverloadedStrings #-}

module Bitcointrade.Ticker (
  Ticker (Ticker),
  getTicker
) where

import           Control.Monad
import           Data.Aeson
import           Network.HTTP.Conduit
import           Format.String
import           Format.Numeric

data Message = Message
  { message :: String
  , ticker  :: Ticker
  } deriving (Show)

instance FromJSON Message where
  parseJSON (Object v) = 
    Message  <$> v .:? "message" .!= "null" 
             <*> v .: "data"
             
  parseJSON _ = mzero

data Ticker = Ticker
  { high            :: Double
  , low             :: Double
  , volume          :: Double
  , trades_quantity :: Integer
  , last            :: Double
  , sell            :: Double
  , buy             :: Double
  , date            :: String
  } 

format = rpad 16 ' '
vFormat = lpad 

instance Show Ticker where
  show (Ticker high low volume trades_quantity lastTrade sell buy date ) =
    format "high"            ++ ": R$ "  ++ (showDouble high)            ++ "\n" ++ 
    format "low"             ++ ": R$ "  ++ (showDouble low)             ++ "\n" ++
    format "volume"          ++ ": BTC " ++ (showDouble volume)          ++ "\n" ++
    format "trades_quantity" ++ ": "     ++ (show       trades_quantity) ++ "\n" ++
    format "lastTrade"       ++ ": R$ "  ++ (showDouble lastTrade)       ++ "\n" ++
    format "sell"            ++ ": R$ "  ++ (showDouble sell)            ++ "\n" ++
    format "buy"             ++ ": R$ "  ++ (showDouble buy)             ++ "\n" ++
    format "date"            ++ ": "     ++ (show       date)            ++ "\n" 

instance FromJSON Ticker where
  parseJSON (Object v) =
    Ticker <$> v .: "high" <*> 
    v .: "low" <*> 
    v .: "volume" <*>
    v .: "trades_quantity" <*>
    v .: "last" <*>
    v .: "sell" <*>
    v .: "buy" <*>
    v .: "date"

  parseJSON _ = mzero

getTicker :: IO (Either String Ticker)
getTicker = do
  response <- simpleHttp "https://api.bitcointrade.com.br/v1/public/BTC/ticker"
  
  return $
    case (eitherDecode response :: Either String Message) of
      Left err                 -> Left err
      Right (Message _ ticker) -> Right ticker
