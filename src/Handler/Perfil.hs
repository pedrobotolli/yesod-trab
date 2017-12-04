{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Perfil where

import Import
import Text.Lucius
import Text.Julius
import Text.Hamlet
import Network.HTTP.Types.Status
import Database.Persist.Postgresql
import Data.Time
import Yesod.Form
import Yesod.Form.Bootstrap3


        
formPerfilPrest :: Prestador -> Html -> MForm Handler (FormResult (Prestador, PrestProfi,FileInfo), Widget)
formPerfilPrest prest = renderBootstrap $ (,,) -- (\a b c d e f g h i j-> (Prestador a b c d e g h f , PrestProfi j))
    <$>
    (Prestador
    <$> areq textField (bfs ("Cpf: " ::Text)) (Just $ prestadorCpfPrest prest)   -- a
    <*> areq passwordField (bfs ("Senha: " :: Text)) (Just $  prestadorSenhaPrest prest) -- b
    <*> areq textField (bfs ("Nome: " ::Text)) (Just $  prestadorNomePrest prest) -- c
    <*> areq emailField (bfs ("Email: " ::Text)) (Just $  prestadorEmailPrest prest) -- d
    <*> areq textField (bfs ("Telefone:" ::Text)) (Just $  prestadorTelPrest prest) -- e 
    <*> areq textField (bfs ("Curriculo:" ::Text)) (Just $  prestadorCurriculoPrest prest) -- f 
    <*> areq textField (bfs ("Cep: " ::Text)) (Just $  prestadorCepPrest prest) -- g
    <*> areq textField (bfs ("Numero: " ::Text)) (Just $  prestadorNumEndPrest prest) -- h
    <*> pure True
    <*> pure (pack $ show $ prestadorFotoPrest prest)
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

getPerfilR :: PrestadorId -> Handler Html
getPerfilR pid = do
    prestador <- runDB $ get404 pid
    prestprof' <- runDB $ selectList [PrestProfiPrestadorId ==. pid] [] :: Handler [Entity PrestProfi]
    prestprof <- return $ fmap (\(Entity _ prof) -> prof) prestprof' --Handler [PrestProfi]
    profids <- return $ fmap prestProfiProfissaoId prestprof -- [Handler Profissao]
    profissoes <- sequence $ fmap (\prid -> runDB $ get404 prid) profids --Handler [Profissao]
    defaultLayout $ do
        setTitle "Service Provider Finder"
        $(whamletFile "templates/perfil.hamlet")
        
getAlteracaogetR :: Handler Html
getAlteracaogetR = do
    defaultLayout $ do
        setTitle "Service Provider Finder"
        $(whamletFile "templates/alteracao.hamlet")
        

    
postAlteracaoR :: PrestadorId -> Handler Html
postAlteracaoR pid = do
    pr <- runDB $ get404 pid;
    ((result,_),_) <- runFormPost $ formPerfilPrest pr
    case result of
        FormSuccess (prestador, prestpr, arq) -> do
            liftIO $ fileMove arq ("static/" Import.++ (unpack $ fileName arq))
            _ <- runDB $ get404 pid
            runDB $ replace pid prestador
            runDB $ deleteWhere [PrestProfiPrestadorId ==. pid]
            runDB $ insert $ PrestProfi (prestProfiProfissaoId prestpr) pid
            runDB $ update pid [PrestadorFotoPrest =."/static/" Import.++ fileName arq]
            redirect $ PerfilR $ pid
        _ -> redirect HomeR
    
    
  


getPerfilPrestR :: PrestadorId -> Handler Html
getPerfilPrestR pid = do
    prestador <- runDB $ get404 pid
    prestprof' <- runDB $ selectList [PrestProfiPrestadorId ==. pid] [] :: Handler [Entity PrestProfi]
    prestprof <- return $ fmap (\(Entity _ prof) -> prof) prestprof' --[Handler PrestProfiId]
    profids <- return $ fmap prestProfiProfissaoId prestprof -- [Handler Profissao]
    profissoes <- sequence $ fmap (\prid -> runDB $ get404 prid) profids --Handler [Profissao]
    (widget, enctype) <- generateFormPost $ formPerfilPrest prestador
    defaultLayout $ do
        setTitle "Service Provider Finder"
        $(whamletFile "templates/perfilprest.hamlet")




