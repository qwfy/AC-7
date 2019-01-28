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

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Control.Monad.IO.Class (liftIO)

import Servant

-- Documentation:
-- https://haskell-servant.readthedocs.io/en/stable/tutorial/ApiType.html

start :: IO ()
start =
  -- TODO @incomplete: Make this port configurable
  run 7710 app

app :: Application
app = serve api server

api :: Proxy Server.API
api = Proxy

type API
  = APIEcho

server :: Server API
server =
  apiEcho

-- Below are the APIs

-- The echo test
type APIEcho = "echo" :> QueryParam' '[Required] "content" String :> Get '[PlainText] String

apiEcho :: String -> Servant.Handler String
apiEcho content = liftIO $ do
  return content
