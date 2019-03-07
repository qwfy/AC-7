{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Server
  ( start
  ) where

import GHC.Generics

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Control.Monad.IO.Class (liftIO)

import Servant
import Data.Aeson
import Data.UUID (UUID)

import NEAT.StorePg
import Database.PostgreSQL.Simple as Pg
import Database.PostgreSQL.Simple.FromRow (FromRow, fromRow, field)

-- Documentation:
-- https://haskell-servant.readthedocs.io/en/stable/tutorial/ApiType.html

start :: IO ()
start = do
  conn <- NEAT.StorePg.getConnection
  -- TODO @incomplete: Make this port configurable
  run 7710 (app conn)

app :: Pg.Connection -> Application
app conn = serve api (server conn)

api :: Proxy Server.API
api = Proxy

type API
  = APIEcho
  :<|> APIGetGenome


server :: Pg.Connection -> Server API
server conn =
  apiEcho
  :<|> apiGetGenome conn

-- Below are the APIs

-- The echo test
type APIEcho = "echo" :> QueryParam' '[Required] "content" String :> Get '[PlainText] String

apiEcho :: String -> Servant.Handler String
apiEcho content = liftIO $ do
  return content


-- Get a genome
data Genome = Genome
  -- TODO @incomplete: change this to UUID
  { genomeId :: UUID
  , generationId :: UUID
  -- TODO @incomplete: confirm that the source of this number is a double
  , originalFitness :: Double
  , speciesSn :: Int
  , graph :: String
  } deriving (Generic)

instance FromRow Genome where
  fromRow = Genome <$> field <*> field <*> field <*> field <*> field

instance ToJSON Genome

type APIGetGenome = "get_genome" :> QueryParam' '[Required] "genome_id" UUID :> Get '[JSON] (Maybe Genome)
apiGetGenome :: Pg.Connection -> UUID -> Servant.Handler (Maybe Genome)
apiGetGenome conn genomeId = do
  let sql = "\
        \select genome_id, generation_id, original_fitness, species_sn, graph \
        \from genome \
        \where genome_id = ?"
  rows <- liftIO $ Pg.query conn sql (Only genomeId)
  case rows of
    [] -> return Nothing
    [row] -> return (Just row)
    _ -> error "More than one rows are returned, this violates the unique constraint"
