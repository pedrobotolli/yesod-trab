{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Index where

import Import
import Text.Lucius
-- import Network.HTTP.Types.Status
-- import Database.Persist.Postgresql

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        addStylesheet $ StaticR css_freelancer_css
        setTitle "Service Provider Finder"
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
    defaultLayout $ do
        addStylesheet $ StaticR css_freelancer_css
        setTitle "Service Provider Finder"
        $(whamletFile "templates/cad-prest.hamlet")
        
postPrestadorR :: Handler TypedContent
postPrestadorR = do 
        prestador <- (requireJsonBody :: Handler Prestador)
        prestadorId <- runDB $ insert prestador
        sendStatusJSON created201 $ object ["PrestadorId".=prestadorId]


getNovaSenhaR :: Handler Html
getNovaSenhaR = do
    defaultLayout $ do
        addStylesheet $ StaticR css_freelancer_css
        setTitle "Service Provider Finder"
        $(whamletFile "templates/novasenha.hamlet")
        
getDenunciaR :: Handler Html
getDenunciaR = do
    defaultLayout $ do
        addStylesheet $ StaticR css_freelancer_css
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
        addStylesheet $ StaticR css_freelancer_css
        setTitle "Service Provider Finder"
        $(whamletFile "templates/paglogin.hamlet")
    
getBuscaR :: Handler Html
getBuscaR = do
    defaultLayout $ do
        addStylesheet $ StaticR css_freelancer_css
        setTitle "Service Provider Finder"
        $(whamletFile "templates/busca.hamlet")
        
getPerfilR :: Handler Html
getPerfilR = do
    defaultLayout $ do
        addStylesheet $ StaticR css_freelancer_css
        setTitle "Service Provider Finder"
        $(whamletFile "templates/perfil.hamlet")
        
getContatoR :: Handler Html
getContatoR = do
    defaultLayout $ do
        addStylesheet $ StaticR css_freelancer_css
        setTitle "Service Provider Finder"
        $(whamletFile "templates/contato.hamlet")
