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

getAddProfiR :: Handler Html
getAddProfiR = do
    (widget, enctype) <- generateFormPost formAddProf
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
                    <form action=@{addProfiR} method=post enctype=#{enctype}>
                        ^{widget}
                    <br>
                                
                    <input type="submit" class="btn btn-primary" value="Enviar">
        
        |]
        
formAddProf :: Html -> MForm Handler (FormResult PrestProfi), Widget)
formAddProf = renderBootstrap $
    <$> areq (selectField $ optionsPersistKey [] [Asc ProfissaoNomeProfissao] profissaoNomeProfissao) (bfs ("Profissão: " ::Text)) Nothing 
    <*> pure (toSqlKey 0)
    