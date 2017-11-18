{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Layout where

import Text.Lucius

myLayout :: Widget -> Handler Html
myLayout widget = do
    pc <- widgetToPageContent $ do
        widget
        toWidget [lucius| body { font-family: verdana } |]
    withUrlRenderer
        [hamlet|
            $doctype 5
            <html>
                <head>
                    <title>#{pageTitle pc}
                    <meta charset=utf-8>
                    <link rel="icon" href="favicon.png" type="image/x-icon">
                    <link href=@{StaticR css_freelancer_css} rel="stylesheet">
                    <link href=@{StaticR css_bootstrap_css} rel="stylesheet">
                    <link href=@{StaticR css_font_awesome_css} rel="stylesheet" type="text/css">
                    <link href=@{StaticR css_fontemont_css} rel="stylesheet" type="text/css">
                    <link href=@{StaticR css_fontelato_css} rel="stylesheet" type="text/css">
                    ^{pageHead pc}
                <body>
                    ^{pageBody pc}
                    <footer class="text-center">
                        <div class="footer-below">
                            <div class="container">
                                <div class="row">
                                    <div class="col-12">
                                        Copyright SPF Corporation
                    <!-- jQuery -->
                    <script src=@{StaticR js_jquery_min_js}>
                
                    <!-- Bootstrap Core JavaScript -->
                    <script src=@{StaticR js_bootstrap_min_js}>
                
                
                    <!-- Plugin JavaScript -->
                    <script src=@{StaticR js_jquery_easing_min_js}>
                
                        <!-- Theme JavaScript -->
                    <script src=@{StaticR js_freelancer_min_js}>
        |]