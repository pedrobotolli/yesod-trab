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
    prepro <- runDB $ selectList [] [] :: Handler [Entity PrestProfi]
    
    prestprofi <- return $ fmap (\(Entity _ pre) -> pre) prepro --Handler [Prestprofi]
    prestids <- return $ fmap prestProfiPrestadorId prestprofi --[Handler ProfissaoId]
    prestadores <- sequence $ fmap (\prestid -> runDB $ get404 prestid) prestids -- Handler [Profissoes]
    
    profids <- return $ fmap prestProfiProfissaoId prestprofi --[Handler ProfissaoId]
    profissoes <- sequence $ fmap (\profid -> runDB $ get404 profid) profids -- Handler [Profissoes]
    prestadorprofissao <- return $ zip3 prestadores profissoes prestids

    sendStatusJSON ok200 (object ["resp" .= (toJSON prestadorprofissao)])