{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Perfil where

import Import
import Text.Lucius
import Text.Julius
import Text.Hamlet
import Network.HTTP.Types.Status
import Database.Persist.Postgresql
import Data.Time
import Yesod.Form
import Yesod.Form.Bootstrap3

getPerfilR :: PrestadorId -> Handler Html
getPerfilR pid = do
    prestador <- runDB $ get404 pid
    prestprof' <- runDB $ selectList [PrestProfiPrestadorId ==. pid] [] :: Handler [Entity PrestProfi]
    prestprof <- return $ fmap (\(Entity _ prof) -> prof) prestprof' --Handler [PrestProfi]
    profids <- return $ fmap prestProfiProfissaoId prestprof -- [Handler Profissao]
    profissoes <- sequence $ fmap (\prid -> runDB $ get404 prid) profids --Handler [Profissao]
    defaultLayout $ do
        setTitle "Service Provider Finder"
        $(whamletFile "templates/perfil.hamlet")
        
getAlteracaogetR :: Handler Html
getAlteracaogetR = do
    defaultLayout $ do
        setTitle "Service Provider Finder"
        $(whamletFile "templates/alteracao.hamlet")
        
patchAlteracaoR :: PrestadorId -> Text -> Handler Value
patchAlteracaoR pid texto = do
    _ <- runDB $ get404 pid
    runDB $ update pid [PrestadorNomePrest =. texto]
    sendStatusJSON noContent204 (object ["resp" .= (fromSqlKey pid)])
    
putAlteracaoR :: PrestadorId -> Handler Value
putAlteracaoR pid = do
    _ <- runDB $ get404 pid
    alteraPrestador <- requireJsonBody :: Handler Prestador
    runDB $ replace pid alteraPrestador
    sendStatusJSON noContent204 (object ["resp" .= (fromSqlKey pid)])
    
        
getPerfilPrestR :: PrestadorId -> Handler Html
getPerfilPrestR pid = do
    prestador <- runDB $ get404 pid
    prestprof' <- runDB $ selectList [PrestProfiPrestadorId ==. pid] [] :: Handler [Entity PrestProfi]
    prestprof <- return $ fmap (\(Entity _ prof) -> prof) prestprof' --[Handler PrestProfiId]
    profids <- return $ fmap prestProfiProfissaoId prestprof -- [Handler Profissao]
    profissoes <- sequence $ fmap (\prid -> runDB $ get404 prid) profids --Handler [Profissao]
    defaultLayout $ do
        setTitle "Service Provider Finder"
        $(whamletFile "templates/perfilprest.hamlet")


formDen :: PrestadorId -> Form Denuncia 
formDen prestadorId = renderBootstrap $ Denuncia 
    <$> areq textareaField (bfs ("Descreva sua denúncia: " ::Text)) Nothing
    <*> fmap utctDay (lift $ liftIO getCurrentTime)
    <*> pure prestadorId

getDenunciaR :: PrestadorId -> Handler Html
getDenunciaR pid = do
    prestador <- runDB $ get404 pid
    (widget,enctype) <- generateFormPost $ formDen pid
    defaultLayout $ do
        setTitle "Service Provider Finder"
        $(whamletFile "templates/denuncia.hamlet")
        
-- UPDATE Cliente SET cliente.nome = nome WHERE cliente.id == cid
patchDenunciaR :: PrestadorId -> Handler Value
patchDenunciaR pid = do 
    _ <- runDB $ get404 pid
    runDB $ update pid [PrestadorContaAtivadaPrest =. False]
    sendStatusJSON noContent204 (object ["resp" .= (fromSqlKey pid)])