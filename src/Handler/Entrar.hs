{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Entrar where

import Import
-- import Network.HTTP.Types.Status
-- import Database.Persist.Postgresql

getEntrarR :: Handler Html
getEntrarR = defaultLayout $ do
    addStylesheet $ StaticR css_freelancer_css
    setTitle "Service Provider Finder"
    [whamlet|
        <head>
            <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css">
            <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js">
            <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js">
            
        <body id="page-top" class="index">
            <div id="skipnav">
                <a href="#maincontent">
                    Skip to main content
                
            <nav id="mainNav" class="navbar navbar-default navbar-fixed-top navbar-custom">
                <div class="container">
                    <div class="navbar-header page-scroll">
                        <button type="button" class="navbar-toggle" data-toggle="collapse" data-target="#bs-example-navbar-collapse-1">
                            <span class="sr-only">Toggle navigation Menu <i class="fa fa-bars">
                        <a class="navbar-brand" href="#page-top">SPF
                    <div class="collapse navbar-collapse" id="bs-example-navbar-collapse-1">
                        <ul class="nav navbar-nav navbar-right">
                            <li>
                                <a href="index">
                                    Inicio
                            <li>
                                <a href="entrar">
                                    Minha Conta
                            <li>
                                <a href="cadastro">
                                    Cadastre-se
                            <li>
                                <a href="contato ">
                                    Contato
                                    
            <header>
                
    |]