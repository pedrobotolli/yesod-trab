{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Foundation where

import Text.Hamlet
import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Yesod.Core.Types     (Logger)

data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static 
    , appConnPool    :: ConnectionPool 
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }
mkYesodData "App" $(parseRoutesFile "config/routes")
type Form a = Html -> MForm Handler (FormResult a, Widget)
instance Yesod App where
    makeLogger = return . appLogger
    defaultLayout widget =  do
        pc <- widgetToPageContent $ do
            widget
        withUrlRenderer $(hamletFile "templates/layout.hamlet")
    authRoute _ = Just LoginR
    isAuthorized (StaticR _) _ = return Authorized
    isAuthorized ListarR _ = return Authorized
    isAuthorized LoginR _ = return Authorized
    isAuthorized HomeR _ = return Authorized
    isAuthorized PrestadorR _ = return Authorized
    isAuthorized (PerfilR _) _ = return Authorized
    isAuthorized ContatoR _ = return Authorized
    isAuthorized AdmR _ = ehAdmin
    isAuthorized _ _ = ehUsuario
    

ehAdmin :: Handler AuthResult
ehAdmin = do 
    sess <- lookupSession "_NOME"
    case sess of 
        Nothing -> return AuthenticationRequired
        Just "admin" -> return Authorized
        Just _ -> return $ Unauthorized "Você não "

ehUsuario :: Handler AuthResult
ehUsuario = do 
    sess <- lookupSession "_NOME"
    case sess of 
        Nothing -> return AuthenticationRequired
        Just _ -> return Authorized

  
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance HasHttpManager App where
    getHttpManager = appHttpManager