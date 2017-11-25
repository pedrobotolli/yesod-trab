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
import Prelude
import Yesod.Form


formPrestador :: Html -> MForm Handler (FormResult (Prestador, PrestProfi), Widget)
formPrestador = renderBootstrap $ (,) -- (\a b c d e f g h i j-> (Prestador a b c d e g h f , PrestProfi j))
    <$>
    (Prestador
    <$> areq textField "Cpf: " Nothing -- a
    <*> areq passwordField "Senha: " Nothing -- b
    <*> areq textField "Nome: " Nothing -- c
    <*> areq emailField "Email: " Nothing -- d
    <*> areq textField "Telefone:" Nothing -- e 
    <*> areq textField "Curriculo:" Nothing -- f 
    <*> areq textField "Cep: " Nothing -- g
    <*> areq textField "Numero: " Nothing -- h
    )
    <*>
    (PrestProfi
    <$> areq (selectField $ optionsPersistKey [] [Asc ProfissaoNomeProfissao] profissaoNomeProfissao) "Profissão: " Nothing -- j
    <*> areq hiddenField "" (Just $ toSqlKey 0)
    )
      
{-    
formProfissao :: Html -> MForm Handler (FormResult PrestProfi, Widget)
formProfissao = renderBootstrap3 $ PrestProfi
    <$> areq (selectFieldList profissoes) "Profissão: " Nothing
  where
    profissoes :: GHandler App App (OptionList ProfissaoId)
    profissoes = do
        entities <- runDB $ selectList [] [Asc ProfissaoNomeProfissao]
        optionsPairs $ Prelude.map (\prof -> (profissaoNomeProfissao $ entityVal prof, entityKey prof)) entities

-}
        

    
getPrestadorR :: Handler Html
getPrestadorR = do
    (widget, enctype) <- generateFormPost formPrestador
    defaultLayout $ do
        [whamlet|
            <br><br>
           <form action=@{PrestadorR} method=post enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="Enviar">
        |]
        
postPrestadorR :: Handler Html
postPrestadorR = do
    ((result,_),_) <- runFormPost formPrestador
    case result of
        FormSuccess (prestador, prestpr) -> do
            prestadorId <- runDB $ insert $ prestador
            runDB $ insert $ PrestProfi (prestProfiProfissaoId prestpr) prestadorId
            redirect PrestadorR
        _ -> redirect HomeR


{-
postPrestadorR :: Handler Html
postPrestadorR = do
    ((result,_),_) <- runFormPost formCadastro
    case result of
        FormSuccess prestador -> do
            runDB $ insert prestador
            redirect PrestadorR
       _ -> redirect HomeR
-}
{-
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
-}