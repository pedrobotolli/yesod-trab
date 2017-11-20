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
        toWidgetHead [hamlet|
        <script async defer src="https://maps.googleapis.com/maps/api/js?key=AIzaSyAeOjMtwy0vXBK5MlFaU4wxf8qRV_ys7Gk&callback=initMap">
        |]
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


data TipoCadastro = TipoCadastro
    { cpf :: Text
    , senha :: Text
    , nome :: Text
    , email :: Text
    , telefone :: Text
    , profissao :: Text
    , curriculo :: Text
    , cep :: Text
    , numero :: Text
    } deriving Show

--data TipoPrestProfi = TipoPrestProfi
--    { cpf :: Text
--    , profissao :: Text
--    } deriving Show



--formCadastro :: Html -> MForm Handler (FormResult TipoCadastro, Widget)
--formCadastro = renderDivs $ TipoCadastro
--   <$> areq textField "Cpf: " Nothing
--    <*> areq passwordField "Senha: " Nothing
--    <*> areq textField "Nome: " Nothing
--    <*> areq emailField "Email: " Nothing
--    <*> areq textField "Telefone:" Nothing
--    <*> areq (selectFieldList profissao) "Profissao: " Nothing
--    <*> areq textField "Curriculo:" Nothing
--    <*> areq textField "Cep: " Nothing
--    <*> areq textField "Numero: " Nothing
--    where
--        profissao :: [(nomeProfissao, ProfissaoId)]
--        profissao = do
--            buscaprof <- runDB $ selectList [] [] :: Handler [Entity Profissao]
--            options <- map (\prof -> (profissaoNomeProfissao $ entityVal prof, fromSqlKey $ entityKey prof)) buscaprof
--            return options


    


    
--getPrestadorR :: Handler Html
--getPrestadorR = do
--   (widget, enctype) <- generateFormPost formCadastro
--    defaultLayout $ do
--        [whamlet|
--            <br><br>
--           <form action=@{PrestadorR} method=post enctype=#{enctype}>
--                ^{widget}
--                <input type="submit" value="Enviar">
--        |]

--postPrestadorR :: Handler Html
--postPrestadorR = do
--    ((result,_),_) <- runFormPost formCadastro
--    case result of
--        FormSuccess prestador -> do
--            runDB $ insert prestador
--            redirect PrestadorR
--       _ -> redirect HomeR



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


getNovaSenhaR :: Handler Html
getNovaSenhaR = do
    defaultLayout $ do
        setTitle "Service Provider Finder"
        $(whamletFile "templates/novasenha.hamlet")
        
getDenunciaR :: Handler Html
getDenunciaR = do
    defaultLayout $ do
        setTitle "Service Provider Finder"
        $(whamletFile "templates/denuncia.hamlet")
        
postDenunciaR :: Handler TypedContent
postDenunciaR = do 
        denuncia <- (requireJsonBody :: Handler Denuncia)
        denunciaId <- runDB $ insert denuncia
        sendStatusJSON created201 $ object ["PrestadorId".=denunciaId]
        
getPagLoginR :: Handler Html
getPagLoginR = do
    defaultLayout $ do
        setTitle "Service Provider Finder"
        $(whamletFile "templates/paglogin.hamlet")
    
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
        
getPerfilR :: PrestadorId -> Handler Html
getPerfilR pid = do
    prestador <- runDB $ get404 pid
    lista' <- runDB $ selectList [PrestProfiPrestadorId ==. pid] [] :: Handler [Entity PrestProfi]
    lista <- return $ fmap (\(Entity _ prof) -> prof) lista'
    profids <- return $ fmap prestProfiProfissaoId lista
    profissoes <- sequence $ fmap (\prid -> runDB $ get404 prid) profids
    defaultLayout $ do
        setTitle "Service Provider Finder"
        $(whamletFile "templates/perfil.hamlet")

        
getContatoR :: Handler Html
getContatoR = do
    defaultLayout $ do
        setTitle "Service Provider Finder"
        $(whamletFile "templates/contato.hamlet")
        
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
    
getRecuperacaoR :: Handler Html
getRecuperacaoR = do
    defaultLayout $ do
        setTitle "Service Provider Finder"
        $(whamletFile "templates/recuperacao.hamlet")
        
getPerfilPrestR :: PrestadorId -> Handler Html
getPerfilPrestR pid = do
    prestador <- runDB $ get404 pid
    sendStatusJSON ok200 (object ["resp" .= (toJSON prestador)])
    defaultLayout $ do
        setTitle "Service Provider Finder"
        $(whamletFile "templates/perfilprest.hamlet")
        
        
getAdmR :: Handler Html
getAdmR = do
    defaultLayout $ do
        setTitle "Service Provider Finder"
        $(whamletFile "templates/adm.hamlet")
    
