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
        
getRemoverProfiR :: PrestadorId -> Handler Html
getRemoverProfiR pid = do
    defaultLayout $ do
        setTitle "Service Provider Finder"
        [whamlet|
            <section id="portfolio">
                <div class="container">
                    <div class="row text-center">
                        <h2>Denúncias
                        <legend>
                    
                    <div class="col-md-8 col-md-offset-2 table-responsive">  
                        <table class="table table-bordered">
                            <thead>
                                <tr>
                                    <td> 
                                        Nome do Prestador
                                    <td>
                                        Data da Denúncia
                                    <td>
                                        Descrição da Denúncias
                            <tbody>
                                $forall (Entity did denuncia, prestador) <- prestadordenuncia
                                    <tr id=#{show $fromSqlKey did}> 
                                        <td>#{prestadorNomePrest prestador}
                                        <td>#{show $ denunciaDtDenuncia denuncia}
                                        <td>#{denunciaDsDenuncia denuncia}
                                        <td><buttom type="buttom" onclick="deletarPrestador(this,'@{DenunciaR $ denunciaPrestadorId denuncia}')"  class="btn btn-danger" >Banir 
            
                          
                          
                        <a class="btn btn-secondary btn-lg pull-right" href=@{ListaContatoR}>Mensagens


        
        |]