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