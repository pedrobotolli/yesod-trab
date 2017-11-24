{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Prestador where

import Import
import Text.Lucius
import Text.Julius
import Text.Hamlet
import Network.HTTP.Types.Status
import Database.Persist.Postgresql


getPrestadorR :: Handler Html
getPrestadorR = do 
    buscaprof <- runDB $ selectList [] [] :: Handler [Entity Profissao]
    defaultLayout $ do
        setTitle "Service Provider Finder"
        toWidgetHead [hamlet|
        <script src="/static/js/jquery.min.js">
        |]
        toWidget $(juliusFile "templates/cadprest.julius")
        $(whamletFile "templates/cad-prest.hamlet")
        
postPrestadorR :: Handler TypedContent
postPrestadorR = do 
    prestador <- (requireJsonBody :: Handler Prestador)
    prestadorId <- runDB $ insert prestador
    sendStatusJSON created201 $ object ["PrestadorId".=prestadorId]
    
postPrestProfiR :: Handler TypedContent
postPrestProfiR = do 
        prestprofi <- (requireJsonBody :: Handler PrestProfi)
        prestprofiId <- runDB $ insert prestprofi
        sendStatusJSON created201 $ object ["PrestProfiId".=prestprofiId]