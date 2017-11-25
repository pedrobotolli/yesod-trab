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

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        setTitle "Service Provider Finder"
        toWidget $(juliusFile "templates/home.julius")
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


        
getContatoR :: Handler Html
getContatoR = do
    defaultLayout $ do
        setTitle "Service Provider Finder"
        $(whamletFile "templates/contato.hamlet")
        

        
        
getAdmR :: Handler Html
getAdmR = do
    denuncias' <- runDB $ selectList [] [] :: Handler [Entity Denuncia]
    denuncias <- return $ fmap (\(Entity _ den) -> den) denuncias' --Handler [Denuncias]
    prestids <- return $ fmap denunciaPrestadorId denuncias -- [Handler PrestadorId]
    prestadores <- sequence $ fmap (\pid -> runDB $ get404 pid) prestids -- Handler [Prestador]
    defaultLayout $ do
        setTitle "Service Provider Finder"
        $(whamletFile "templates/adm.hamlet")
        
{-
getNovaSenhaR :: Handler Html
getNovaSenhaR = do
    defaultLayout $ do
        setTitle "Service Provider Finder"
        $(whamletFile "templates/novasenha.hamlet")

getRecuperacaoR :: Handler Html
getRecuperacaoR = do 
    defaultLayout $ do
        setTitle "Service Provider Finder"
        $(whamletFile "templates/recuperacao.hamlet")

getAlteracaogetR :: Handler Html
getAlteracaogetR = do
    defaultLayout $ do
        setTitle "Service Provider Finder"
        $(whamletFile "templates/alteracao.hamlet")

putAlteracaoR :: PrestadorId -> Handler Value
putAlteracapR pid = do
    _ <- runDB $ get404 pid
    novoPrestador <- requireJsonBody :: Handler Prestador
    runDB $ replace pid novoPrestador
    sendStatusJSON noContent204 (object ["resp" .= (fromSqlKey pid)])
    
getPerfilR :: PrestadorId -> Handler Html
getPerfilR = do
    prestador <- runDB $ get404 pid
    defaultLayout $ do
        setTitle "Service Provider Finder"
        $(whamletFile "templates/perfil.hamlet")

getPerfilPrestR :: PrestadorId -> Handler Html
getPerfilR = do
    prestador <- runDB $ get404 pid
    defaultLayout $ do
        setTitle "Service Provider Finder"
        $(whamletFile "templates/perfilprest.hamlet")

getPagLoginR :: Handler Html
getPagLoginR = do
    defaultLayout $ do
        setTitle "Service Provider Finder"
        $(whamletFile "templates/paglogin.hamlet")
-}
