{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell,
             OverloadedStrings, GADTs, FlexibleContexts, MultiParamTypeClasses #-}
module Main where

import Yesod
import Yesod.Static
import Yesod.Form
import Yesod.Form.Jquery

import MercadosCampesinos.Model.Market (MarketType(..), marketTypeLabel)

import           Data.Maybe (fromMaybe, fromJust)
import           Control.Applicative
import           Control.Arrow ((&&&))
import           Control.Monad.Trans.Resource (runResourceT)
import           Control.Monad.Logger (runStderrLoggingT)

import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH

import           Data.Time (Day, UTCTime, getCurrentTime)

import           Text.Hamlet (hamletFile)
import           Text.Julius (juliusFile)
import           Text.Lucius (luciusFile)

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Text as T
import           Text.Read (readMaybe)

connStr :: ConnectionString
connStr = "host=localhost dbname=mc user=arpunk password='' port=5432"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Market
    title        T.Text
    description  Textarea
    date         Day
    whatType     MarketType
    address      T.Text
    credibility  Int
    verified     Bool
    lat          Double
    lng          Double
    
Wishlist
    market       MarketId
    what         Textarea
|]

data MercadosCampesinos = MercadosCampesinos {
    connPool  :: ConnectionPool
  , getStatic :: Static  
  }

staticFiles "static/"

mkYesod "MercadosCampesinos" [parseRoutes|
  /static                      StaticR        Static getStatic
  /                            HomeR          GET
  /mercados                    MarketsR       GET
  /mercado/!new                NewMarketR     GET POST
  /mercado/#MarketId           MarketR        GET
  /mercado/#MarketId/pedidos   NewWishlistR   POST
  --
  -- API JSON
  --
  /api/mercados                MarketsJsonR   GET
  /api/mercado/#MarketId       MarketJsonR    GET
|]

instance Yesod MercadosCampesinos where
  defaultLayout widget = do
    master <- getYesod
    mmsg <- getMessage

    pc <- widgetToPageContent $ do
      $(whamletFile "templates/layout.hamlet")

      addScript $ StaticR js_angular_min_js
      addScript $ StaticR js_jquery_min_js
      addScript $ StaticR js_bootstrap_min_js

    giveUrlRenderer $(hamletFile "templates/wrapper.hamlet")

instance YesodPersist MercadosCampesinos where
  type YesodPersistBackend MercadosCampesinos = SqlPersistT

  runDB action = do
    MercadosCampesinos pool _ <- getYesod
    runSqlPool action pool

instance YesodJquery MercadosCampesinos

instance RenderMessage MercadosCampesinos FormMessage where
  renderMessage _ _ = defaultFormMessage

navbarWidget :: Widget
navbarWidget = $(whamletFile "templates/widget/navbar.hamlet")

newMarketForm :: Html -> MForm Handler (FormResult Market, Widget)
newMarketForm = do
  renderDivs $ Market
    <$> areq textField "Titulo" Nothing
    <*> areq textareaField "Descripcion" Nothing
    <*> areq (jqueryDayField def
              { jdsChangeYear = False }) "Fecha de apertura" Nothing
    <*> areq (selectFieldList marketTypes) "Tipo de mercado" Nothing
    <*> areq textField "Direccion" Nothing
    <*> pure 0
    <*> pure False
    <*> areq doubleField (FieldSettings (SomeMessage $ T.pack "Latitud") Nothing (Just "lat") Nothing []) Nothing
    <*> areq doubleField (FieldSettings (SomeMessage $ T.pack "Longitud") Nothing (Just "lng") Nothing []) Nothing
    where
      marketTypes = map (T.pack . marketTypeLabel &&& id) [DirectBuy .. ]

newWishlistForm :: MarketId -> Html -> MForm Handler (FormResult Wishlist, Widget)
newWishlistForm mid = do
  renderDivs $ Wishlist
    <$> pure mid
    <*> areq textareaField "¿Que desearías encontrar?" Nothing

postNewMarketR :: Handler Html
postNewMarketR = do
  ((form, _), _) <- runFormPost newMarketForm

  case form of
    FormSuccess market -> do
      newMarketId <- runDB $ insert market
      redirect $ MarketR newMarketId

    _ -> redirect HomeR

getNewMarketR :: Handler Html
getNewMarketR = do
  (newMarketWidget, enctype) <- generateFormPost newMarketForm

  defaultLayout $ do
    setTitle "Nuevo mercado"
    toWidget $(juliusFile "templates/new-market.julius")
    toWidget $(luciusFile "templates/new-market.lucius")
    $(whamletFile "templates/new-market.hamlet")

getMarketsR :: Handler Html
getMarketsR = do
  page <- lookupGetWithDefault "page" 0
  per_page <- lookupGetWithDefault "count" 20
  markets <- runDB $ selectList [] [Asc MarketDate, LimitTo per_page, OffsetBy page]

  defaultLayout $ do
    setTitle "Mercados"
    toWidget $(juliusFile "templates/markets.julius")
    toWidget $(luciusFile "templates/markets.lucius")
    $(whamletFile "templates/markets.hamlet")

getMarketR :: MarketId -> Handler Html
getMarketR mid = do
  market <- runDB $ get404 mid
  wishlists <- runDB $ selectList [WishlistMarket ==. mid] []
  
  (newWishlistWidget, wlEncType) <- generateFormPost $ newWishlistForm mid

  defaultLayout $ do
    setTitle "Mercado"
    toWidget $(juliusFile "templates/market.julius")
    toWidget $(luciusFile "templates/market.lucius")
    $(whamletFile "templates/market.hamlet")

postNewWishlistR :: MarketId -> Handler Html
postNewWishlistR mid = do
  ((form, _), _) <- runFormPost $ newWishlistForm mid

  case form of
    FormSuccess nw -> do
      _ <- runDB $ insert nw
      redirect $ MarketR mid
    _ ->
      redirect $ MarketR mid

getHomeR :: Handler Html
getHomeR = do
  markets <- runDB $ selectList [] [Asc MarketDate]
  
  defaultLayout $ do
    setTitle "Inicio"
    toWidget $(juliusFile "templates/home.julius")
    toWidget $(luciusFile "templates/home.lucius")
    $(whamletFile "templates/home.hamlet")

getMarketsJsonR :: Handler Value
getMarketsJsonR = do
  markets <- runDB $ selectList [] [Asc MarketDate, LimitTo 6]
   
  returnJson $ fmap marketToGeoJSON markets

getMarketJsonR :: MarketId -> Handler Value
getMarketJsonR mid = do
  market <- runDB $ get404 mid

  returnJson $ object [("latitude" .= (marketLat market))
                      ,("longitude" .= (marketLng market))]

marketToGeoJSON :: Entity Market -> Value
marketToGeoJSON (Entity _ m) =
  object [("type" .= ("Feature" :: String))
         ,("properties" .= object [("name" .= marketTitle m)
                                  ,("credibility" .= marketCredibility m)
                                  ,("verified" .= marketVerified m)
                                  ,("popupContent" .= (show $ marketDescription m))])
         ,("geometry" .= object [("type" .= ("Point" :: String))
                                ,("coordinates" .=
                                  [ (marketLng m)
                                  , (marketLat m) ])
                                ])
         ]

lookupGetWithDefault :: Read a => T.Text -> a -> Handler a
lookupGetWithDefault name def = do
  mval <- lookupGetParam name
  return $ fromMaybe def $ mval >>= readMaybe . T.unpack

shortenTitle = shortenText 30
shortenDesc d = shortenText 200 $ unTextarea d

shortenText :: Int -> T.Text -> T.Text
shortenText num text
  | T.length text > num = (T.take num text) `T.append` "..."
  | otherwise = text

main :: IO ()
main = withPostgresqlPool connStr 10 $ \pool -> do
  flip runSqlPersistMPool pool $ do
    runMigration migrateAll

  static@(Static settings) <- static "static"
  warp 3000 $ MercadosCampesinos pool static
