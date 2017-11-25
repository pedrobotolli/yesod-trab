{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Autenticacao where

import Import
import Text.Lucius
import Text.Julius
import Text.Hamlet
import Network.HTTP.Types.Status
import Database.Persist.Postgresql

getNovaSenhaR :: Handler Html
getNovaSenhaR = do
    defaultLayout $ do
        setTitle "Service Provider Finder"
        $(whamletFile "templates/novasenha.hamlet")
        
        
getPagLoginR :: Handler Html
getPagLoginR = do
    defaultLayout $ do
        setTitle "Service Provider Finder"
        $(whamletFile "templates/paglogin.hamlet")
        

    
getRecuperacaoR :: Handler Html
getRecuperacaoR = do
    defaultLayout $ do
        setTitle "Service Provider Finder"
        $(whamletFile "templates/recuperacao.hamlet")
        

{-
patchNovaSenhaR :: PrestadorId -> Text -> Handler Value
patchNovaSenhaR pid senha = do
    _ <- runDB $ get404 pid
    runDB $ update pid [PrestadorSenha =. nome]
    sendStatusJSON noContent204 (object ["resp" .= (fromSqlKey pid)])


getLogaR :: Handler Html
getLogaR email senha = do
    logando <- runDB $ get404 email senha
    prestador <- runDB $ selectList [PrestadorEmail ==. email] []
    pid <- return $ fmap prestadorPrestadorId prestador
    redirect PerfilPrestR pid
-}