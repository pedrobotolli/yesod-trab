{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Listar where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql


getListarR :: Handler Value
getListarR = do
    rows <- runDB $ selectList [] [] :: Handler [Entity Prestador]
    sendStatusJSON ok200 (object ["resp" .= (toJSON rows)])