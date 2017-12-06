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
import Network.HTTP.Types.Status
import Database.Persist.Postgresql
import Data.Time
import Yesod.Form
import Yesod.Form.Bootstrap3
getHomeR :: Handler Html
getHomeR = do
    prestadores <- runDB $ selectList [] [] :: Handler [Entity Prestador]
    defaultLayout $ do
        setTitle "Service Provider Finder"
        toWidgetHead $(juliusFile "templates/home.julius")
        $(whamletFile "templates/home.hamlet")
        toWidget $ 
            [lucius|
                #map{
                    height: 550px;
                    width: 100%;
                    }
                #busca{
                    position: absolute;
                    top: 550px;
                }
            |]



{-
formCadastro :: Html -> MForm Handler (FormResult TipoCadastro, Widget)
formCadastro = renderBootstrap $ TipoCadastro
   <$> areq textField "Cpf: " Nothing
    <*> areq passwordField "Senha: " Nothing
    <*> areq textField "Nome: " Nothing
    <*> areq emailField "Email: " Nothing
    <*> areq textField "Telefone:" Nothing
    <*> areq (selectFieldList profissao) "Profissao: " Nothing
    <*> areq textField "Curriculo:" Nothing
    <*> areq textField "Cep: " Nothing
    <*> areq textField "Numero: " Nothing
    where
        profissao :: [(nomeProfissao, ProfissaoId)]
        profissao = do
            buscaprof <- runDB $ selectList [] [] :: Handler [Entity Profissao]
            options <- map (\prof -> (profissaoNomeProfissao $ entityVal prof, fromSqlKey $ entityKey prof)) buscaprof
            return options


    


    
getPrestadorR :: Handler Html
getPrestadorR = do
   (widget, enctype) <- generateFormPost formCadastro
    defaultLayout $ do
        [whamlet|
            <br><br>
           <form action=@{PrestadorR} method=post enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="Enviar">
        |]

postPrestadorR :: Handler Html
postPrestadorR = do
    ((result,_),_) <- runFormPost formCadastro
    case result of
        FormSuccess prestador -> do
            runDB $ insert prestador
            redirect PrestadorR
       _ -> redirect HomeR

-}
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


formCont :: Form Contato
formCont = renderBootstrap $ Contato 
    <$> areq textField (bfs ("Nome: " ::Text)) Nothing -- c
    <*> areq emailField (bfs ("Email: " ::Text)) Nothing -- c
    <*> areq textField (bfs ("Assunto: " ::Text)) Nothing -- c
    <*> areq textareaField (bfs ("Mensagem: " ::Text)) Nothing -- c
    <*> fmap utctDay (lift $ liftIO getCurrentTime)
        
getContatoR :: Handler Html
getContatoR = do
    (widget,enctype) <- generateFormPost $ formCont
    defaultLayout $ do
        setTitle "Service Provider Finder"
        $(whamletFile "templates/contato.hamlet")
        
        
postContatoR :: Handler Html
postContatoR = do
    ((result,_),_) <- runFormPost $ formCont
    case result of
        FormSuccess contato -> do
            runDB $ insert contato
            redirect HomeR
        _ -> redirect HomeR
        

        
        

