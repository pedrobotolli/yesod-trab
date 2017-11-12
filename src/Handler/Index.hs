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
        
    
getCadPrestR :: Handler Html
getCadPrestR = do 
    defaultLayout $ do
        addStylesheet $ StaticR css_freelancer_css
        setTitle "Service Provider Finder"
        $(whamletFile "templates/cad-prest.hamlet")
        
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
