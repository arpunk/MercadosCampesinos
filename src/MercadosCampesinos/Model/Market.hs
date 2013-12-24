{-# LANGUAGE TemplateHaskell #-}
module MercadosCampesinos.Model.Market where

import Yesod
import Prelude

import           Database.Persist.TH

data MarketType = DirectBuy | CoopStore | OtherStore
                deriving (Read, Show, Bounded, Eq, Ord, Enum)

marketTypeLabel :: MarketType -> String
marketTypeLabel DirectBuy = "Compra directa"
marketTypeLabel CoopStore = "Tienda cooperativa"
marketTypeLabel OtherStore = "Otra tienda"

derivePersistField "MarketType"
