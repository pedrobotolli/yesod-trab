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
    defaultLayout $ do
        setTitle "Service Provider Finder"
        $(whamletFile "templates/addprof.hamlet")