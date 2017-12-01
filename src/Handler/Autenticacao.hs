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
import Yesod.Form.Bootstrap3

formLogin :: Form (Text,Text) 
formLogin = renderDivs $ (,) 
    <$> areq emailField (bfs ("Email: " :: Text)) Nothing
    <*> areq passwordField (bfs ("Senha: " :: Text)) Nothing

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
        


patchNovaSenhaR :: PrestadorId -> Text -> Handler Value
patchNovaSenhaR pid senha = do
    _ <- runDB $ get404 pid
    runDB $ update pid [PrestadorSenhaPrest =. senha]
    sendStatusJSON noContent204 (object ["resp" .= (fromSqlKey pid)])

autentica :: Text -> Text -> HandlerT App IO (Maybe (Entity Prestador))
autentica email senha = runDB $ selectFirst [PrestadorEmailPrest ==. email
                                            ,PrestadorSenhaPrest ==. senha] []

postLoginR :: Handler Html
postLoginR = do 
    ((resultado,_),_) <- runFormPost formLogin
    case resultado of
        FormSuccess (email,senha) -> do 
            talvezPrestador <- autentica email senha
            case talvezPrestador of 
                Nothing -> do 
                    setMessage [shamlet|
                        <div> 
                            Prestador nao encontrado/Senha invalida!
                    |]
                    redirect LoginR
                Just (Entity chave pre) -> do 
                    setSession "_NOME" (prestadorNomePrest pre)
                    redirect HomeR
                
        _ -> redirect HomeR
    

getLoginR :: Handler Html
getLoginR = do 
    (widget,enctype) <- generateFormPost formLogin
    mensa <- getMessage
    defaultLayout $ do 
        $(whamletFile "templates/paglogin.hamlet")
        

        
