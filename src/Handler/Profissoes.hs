{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Profissoes where

import Import
import Text.Lucius
import Text.Julius
import Text.Hamlet
import Network.HTTP.Types.Status
import Database.Persist.Postgresql
import Prelude
import Yesod.Form
import Yesod.Form.Bootstrap3

getAdicionarProfiR :: PrestadorId -> Handler Html
getAdicionarProfiR pid = do
    (widget, enctype) <- generateFormPost formAdicionarProf
    defaultLayout $ do
        setTitle "Service Provider Finder"
        [whamlet|
            <section id="portfolio">
                <div class="container">
                    <div class="row">
                        <div class="col-md-12 text-center">
                            <br>
                            <legend>
                                <h2>Adicionar Nova Profissão
                                
                
                <br>
                <div class="container">
                    <form action=@{AdicionarProfiR pid} method=post enctype=#{enctype}>
                        ^{widget}
                    <br>
                                
                    <input type="submit" class="btn btn-primary" value="Enviar">
        
        |]
        
formAdicionarProf :: Html -> MForm Handler (FormResult PrestProfi, Widget)
formAdicionarProf = renderBootstrap $ PrestProfi
    <$> areq (selectField $ optionsPersistKey [] [Asc ProfissaoNomeProfissao] profissaoNomeProfissao) (bfs ("Profissão: " ::Text)) Nothing 
    <*> pure (toSqlKey 0)

postAdicionarProfiR :: PrestadorId -> Handler Html
postAdicionarProfiR pid = do
    ((result,_),_) <- runFormPost formAdicionarProf
    case result of
        FormSuccess profissao -> do
            runDB $ insert $ PrestProfi (prestProfiProfissaoId profissao) pid
            redirect PrestadorR
        _ -> redirect HomeR