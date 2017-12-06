{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Adm where

import Import
import Text.Lucius
import Text.Julius
import Text.Hamlet
import Network.HTTP.Types.Status
import Database.Persist.Postgresql
import Data.Time
import Yesod.Form
import Yesod.Form.Bootstrap3


getAdmR :: Handler Html
getAdmR = do
    denuncias' <- runDB $ selectList [] [] :: Handler [Entity Denuncia]
    denuncias <- return $ fmap (\(Entity _ den) -> den) denuncias' --Handler [Denuncias]
    prestids <- return $ fmap denunciaPrestadorId denuncias -- [Handler PrestadorId]
    prestadores <- sequence $ fmap (\pid -> runDB $ get404 pid) prestids -- Handler [Prestador]
    prestadordenuncia <- return $ zip denuncias' prestadores
    defaultLayout $ do
        setTitle "Service Provider Finder"
        toWidget $(juliusFile "templates/requisicao.julius")
        $(whamletFile "templates/adm.hamlet")
        
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

postDenunciaR :: PrestadorId -> Handler Html
postDenunciaR pid = do
    ((result,_),_) <- runFormPost $ formDen pid
    case result of
        FormSuccess denuncia -> do
            runDB $ insert denuncia
            redirect HomeR
        _ -> redirect HomeR

        
patchDenunciaR :: PrestadorId -> Handler Value
patchDenunciaR pid = do 
    _ <- runDB $ get404 pid
    runDB $ update pid [PrestadorContaAtivadaPrest =. False]
    runDB $ deleteWhere [DenunciaPrestadorId ==. pid]
    sendStatusJSON noContent204 (object ["resp" .= (fromSqlKey pid)])
    
getListaContatoR :: Handler Html
getListaContatoR = do
    mensagens <- runDB $ selectList [] [] :: Handler [Entity Contato]
    defaultLayout $ do
        setTitle "Service Provider Finder"
        toWidget $(juliusFile "templates/requisicaocont.julius")
        $(whamletFile "templates/mensagens.hamlet")
        
deleteDeletaContatoR :: ContatoId -> Handler Value
deleteDeletaContatoR cid = do 
    _ <- runDB $ get404 cid
    runDB $ delete cid
    sendStatusJSON noContent204 (object ["resp" .= (fromSqlKey cid)])
