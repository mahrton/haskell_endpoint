{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators   #-}

module Lib
    ( startApp
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Data.Time.Calendar
import GHC.Generics

data Order = Order 
  { oid:: String
  , customerId :: String
  , customer :: Customer
  , address :: Address
  , card :: Card
  , items :: [Item]
  , shipment :: Shipment
  , date :: Day
  , total :: Float
  } deriving (Eq, Show, Generic)

data Customer = Customer
  { cid:: String
  , firstName:: String
  , lastName:: String
  , username:: String
  , addresses:: [Address]
  , cards :: [Card]
  } deriving (Eq, Show, Generic)

data Address = Address  
 { aid:: String
 , street:: String
 , city:: String
 , postcode:: String
 , country:: String
 } deriving (Eq, Show, Generic)

data Card = Card
 { did :: String
 , longNum :: String
 , expires :: String
 , ccv :: String
 } deriving (Eq, Show, Generic)

data Item = Item
 { iid :: String
 , itemId :: String
 , quantity :: Int
 , unitPrice :: Float
 } deriving (Eq, Show, Generic)

data Shipment = Shipment
 { sid :: String
 , name :: String
 } deriving (Eq, Show, Generic)

dropIdOnly :: String -> String
dropIdOnly label = case label of
                     x : "id" -> "id"
                     _        -> label

instance ToJSON Order where toJSON = genericToJSON defaultOptions { fieldLabelModifier = dropIdOnly }
instance ToJSON Customer where toJSON = genericToJSON defaultOptions { fieldLabelModifier = dropIdOnly }
instance ToJSON Card where toJSON = genericToJSON defaultOptions { fieldLabelModifier = dropIdOnly }
instance ToJSON Address where toJSON = genericToJSON defaultOptions { fieldLabelModifier = dropIdOnly }
instance ToJSON Item where toJSON = genericToJSON defaultOptions { fieldLabelModifier = dropIdOnly }
instance ToJSON Shipment where toJSON = genericToJSON defaultOptions { fieldLabelModifier = dropIdOnly }

type API = "orders" :> Get '[JSON] Order
      :<|> "orders" :> ReqBody '[JSON] Order :> Post '[JSON] String

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = order
        :<|> getOrder

   where order :: Handler Order
         order = return (Order "id" "cusomerid" customer_ address_ card_ [item_] shipment_ (fromGregorian 2015 12 1) 18.90)

         getOrder :: Order -> Handler String
         getOrder order = return "Received Order"

customer_ :: Customer
customer_ = Customer "id" "firstName" "lastName" "userName" [address_] [card_]

address_ :: Address
address_ = Address "id" "street" "city" "postCode" "country"

card_ :: Card
card_ = Card "id" "0000111122223333" "03/21" "453"

item_ :: Item 
item_ = Item "id" "itemId" 2 18.90

shipment_ :: Shipment
shipment_ = Shipment "id" "name"
