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
import Yesod.Form.Bootstrap3


formPrestador :: Html -> MForm Handler (FormResult (Prestador, PrestProfi,FileInfo), Widget)
formPrestador = renderBootstrap $ (,,) -- (\a b c d e f g h i j-> (Prestador a b c d e g h f , PrestProfi j))
    <$>
    (Prestador
    <$> areq textField (bfs ("Cpf: " ::Text)) Nothing -- a
    <*> pure "senhaDoPrestador" -- b
    <*> areq textField (bfs ("Nome: " ::Text)) Nothing -- c
    <*> areq emailField (bfs ("Email: " ::Text)) Nothing -- d
    <*> areq textField (bfs ("Telefone:" ::Text)) Nothing -- e 
    <*> areq textField (bfs ("Curriculo:" ::Text)) Nothing -- f 
    <*> areq textField (bfs ("Cep: " ::Text)) Nothing -- g
    <*> areq textField (bfs ("Numero: " ::Text)) Nothing -- h
    <*> pure False
    <*> areq hiddenField "Foto" Nothing
    )
    <*>
    (PrestProfi
    <$> areq (selectField $ optionsPersistKey [] [Asc ProfissaoNomeProfissao] profissaoNomeProfissao) (bfs ("Profissão: " ::Text)) Nothing -- j
    <*> pure (toSqlKey 0)
    )
    <*>
    (
      areq fileField 
                           FieldSettings{fsId=Just "hident1",
                                         fsLabel="Arquivo: ",
                                         fsTooltip= Nothing,
                                         fsName= Nothing,
                                         fsAttrs=[("accept","image/*")]} 
                           Nothing
        
    )
    
      


    
getPrestadorR :: Handler Html
getPrestadorR = do
    (widget, enctype) <- generateFormPost formPrestador
    defaultLayout $ do
        [whamlet|
            <br>
            <form action=@{PrestadorR} method=post enctype=#{enctype}>
                ^{widget}
                <br>
                                
            <input type="submit" value="Enviar">
        |]
        
postPrestadorR :: Handler Html
postPrestadorR = do
    ((result,_),_) <- runFormPost formPrestador
    case result of
        FormSuccess (prestador, prestpr,arq) -> do
            liftIO $ fileMove arq ("static/" Import.++ (unpack $ fileName arq))
            prestadorId <- runDB $ insert $ prestador
            runDB $ insert $ PrestProfi (prestProfiProfissaoId prestpr) prestadorId
            redirect PrestadorR
        _ -> redirect HomeR

{-formProfissao :: Html -> MForm Handler (FormResult PrestProfi, Widget)
formProfissao = renderBootstrap3 $ PrestProfi
    <$> areq (selectFieldList profissoes) "Profissão: " Nothing
  where
    profissoes :: GHandler App App (OptionList ProfissaoId)
    profissoes = do
        entities <- runDB $ selectList [] [Asc ProfissaoNomeProfissao]
        optionsPairs $ Prelude.map (\prof -> (profissaoNomeProfissao $ entityVal prof, entityKey prof)) entities
        
-}      
{-        
formInd :: PrestadorId -> Form Indicacao
formInd pid = renderBootstrap $ Indicacao
 -}  
    


        
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
        
        
postApagarProdR :: ProdutoId -> Handler Html
postApagarProdR pid = do 
    _ <- runDB $ get404 pid
    runDB $ delete pid 
    redirect ListaProdutoR
-}