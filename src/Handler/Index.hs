{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Index where

import Import
import Text.Lucius
import Text.Julius
import Text.Hamlet
-- import Network.HTTP.Types.Status
import Database.Persist.Postgresql


getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        setTitle "Service Provider Finder"
        toWidgetHead [hamlet|
        <script async defer src="https://maps.googleapis.com/maps/api/js?key=AIzaSyAeOjMtwy0vXBK5MlFaU4wxf8qRV_ys7Gk&callback=initMap">
        |]
        toWidget $(juliusFile "templates/home.julius")
        $(whamletFile "templates/home.hamlet")
        toWidget $ 
            [lucius|
                map{
                    height: 550px;
                    width: 100%;
                    }
                busca{
                    position: absolute;
                    top: 550px;
                }
            |]
      

    
getPrestadorR :: Handler Html
getPrestadorR = do 
    buscaprof <- runDB $ selectList [] [] :: Handler [Entity Profissao]
    defaultLayout $ do
        setTitle "Service Provider Finder"
        toWidgetHead [hamlet|
        <script src="/static/jquery-3.2.1.min.js">
        |]
        toWidget $(juliusFile "templates/cadprest.julius")
        $(whamletFile "templates/cad-prest.hamlet")
        
postPrestadorR :: Handler TypedContent
postPrestadorR = do 
        prestador <- (requireJsonBody :: Handler Prestador)
        prestadorId <- runDB $ insert prestador
        sendStatusJSON created201 $ object ["PrestadorId".=prestadorId]


getNovaSenhaR :: Handler Html
getNovaSenhaR = do
    defaultLayout $ do
        setTitle "Service Provider Finder"
        $(whamletFile "templates/novasenha.hamlet")
        
getDenunciaR :: Handler Html
getDenunciaR = do
    defaultLayout $ do
        setTitle "Service Provider Finder"
        $(whamletFile "templates/denuncia.hamlet")
        
postDenunciaR :: Handler TypedContent
postDenunciaR = do 
        denuncia <- (requireJsonBody :: Handler Denuncia)
        denunciaId <- runDB $ insert denuncia
        sendStatusJSON created201 $ object ["PrestadorId".=denunciaId]
        
getPagLoginR :: Handler Html
getPagLoginR = do
    defaultLayout $ do
        setTitle "Service Provider Finder"
        $(whamletFile "templates/paglogin.hamlet")
    
getBuscaR :: Handler Html
getBuscaR = do
    defaultLayout $ do
        setTitle "Service Provider Finder"
        $(whamletFile "templates/busca.hamlet")
        
postBuscaR :: Handler TypedContent
postBuscaR = do
    busca <- (requireJsonBody :: Handler Prestador)
    buscares <- runDB $ selectList [Filter PrestadorNomePrest (Left $ mconcat ["%",prestadorNomePrest busca,"%"]) (BackendSpecificFilter "ILIKE")] []
    sendStatusJSON ok200 (object ["resp" .= (toJSON buscares)])
        
getPerfilR :: Handler Html
getPerfilR = do
    defaultLayout $ do
        setTitle "Service Provider Finder"
        $(whamletFile "templates/perfil.hamlet")
        
getContatoR :: Handler Html
getContatoR = do
    defaultLayout $ do
        setTitle "Service Provider Finder"
        $(whamletFile "templates/contato.hamlet")
        
getAlteracaogetR :: Handler Html
getAlteracaogetR = do
    defaultLayout $ do
        setTitle "Service Provider Finder"
        $(whamletFile "templates/alteracao.hamlet")
        
--patchAlteracaoR :: PrestadorId -> Text -> Handler Value
--patchAlteracaoR pid texto = do
--       _ <- runDB $ get404 pid
--      runDB $ update pid [PrestadorNomePrest =. texto]
--        sendStatusJSON noContent204 (object ["resp" .= (fromSqlKey pid)])
putAlteracaoR :: PrestadorId -> Handler Value
putAlteracaoR pid = do
    _ <- runDB $ get404 pid
    alteraPrestador <- requireJsonBody :: Handler Prestador
    runDB $ replace pid alteraPrestador
    sendStatusJSON noContent204 (object ["resp" .= (fromSqlKey pid)])
    
getRecuperacaoR :: Handler Html
getRecuperacaoR = do
    defaultLayout $ do
        setTitle "Service Provider Finder"
        $(whamletFile "templates/recuperacao.hamlet")
        
getPerfilPrestR :: Handler Html
getPerfilPrestR = do
    defaultLayout $ do
        setTitle "Service Provider Finder"
        $(whamletFile "templates/perfilprest.hamlet")
        
        
getAdmR :: Handler Html
getAdmR = do
    defaultLayout $ do
        setTitle "Service Provider Finder"
        $(whamletFile "templates/adm.hamlet")
    
