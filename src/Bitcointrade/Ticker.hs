{-# LANGUAGE OverloadedStrings #-}

module Bitcointrade.Ticker where

import           Control.Monad
import           Data.Aeson
import           Network.HTTP.Conduit
import           Format.String

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
  { high            :: Float
  , low             :: Float
  , volume          :: Float
  , trades_quantity :: Integer
  , last            :: Float
  , sell            :: Float
  , buy             :: Float
  , date            :: String
  } 

format = rpad 16 ' '
vFormat = lpad 

instance Show Ticker where
  show (Ticker high low volume trades_quantity lastTrade sell buy date ) =
    format "high"            ++ ": R$ "  ++ (show high)            ++ "\n" ++ 
    format "low"             ++ ": R$ "  ++ (show low)             ++ "\n" ++
    format "volume"          ++ ": BTC " ++ (show volume)          ++ "\n" ++
    format "trades_quantity" ++ ": "     ++ (show trades_quantity) ++ "\n" ++
    format "lastTrade"       ++ ": R$ "  ++ (show lastTrade)       ++ "\n" ++
    format "sell"            ++ ": R$ "  ++ (show sell)            ++ "\n" ++
    format "buy"             ++ ": R$ "  ++ (show buy)             ++ "\n" ++
    format "date"            ++ ": "     ++ (show date)            ++ "\n" 

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

getLast :: Ticker -> Float
getLast (Ticker _ _ _ _ lastTrade _ _ _) = lastTrade

getTicker :: IO (Either String Ticker)
getTicker = do
  response <- simpleHttp "https://api.bitcointrade.com.br/v1/public/BTC/ticker"
  
  return $
    case (eitherDecode response :: Either String Message) of
      Left err                 -> Left err
      Right (Message _ ticker) -> Right ticker
