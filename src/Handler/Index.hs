{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Index where

import Import
import Foundation
import Yesod.Core
-- import Network.HTTP.Types.Status
-- import Database.Persist.Postgresql

getIndexR :: Handler Html
getIndexR = defaultLayout $ do
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
                            <span class="sr-only">Toggle navigation</span> Menu <i class="fa fa-bars">
                        <a class="navbar-brand" href="#page-top">SPF</a>
                    <div class="collapse navbar-collapse" id="bs-example-navbar-collapse-1">
                        <ul class="nav navbar-nav navbar-right">
                            <li>
                                <a href="index">Inicio</a>
                            <li>
                                <a href="entrar">Minha Conta</a>
                            <li>
                                <a href="cadastro">Cadastre-se</a>
                            <li>
                                <a href="contato">Contato</a>
            <header>
                <div id="map">
                <script>
                    function initMap() {
                        var geocoder = new google.maps.Geocoder();
                        var endereco ="Guarujá, São Paulo, Brasil";
                        var posicao;
                        var marcador = "https://i.imgur.com/llVkbck.png"
                        var medialat=0;
                        var medialong=0;
        
                        var nome= "Marcos";
                        var servico = "Eletricista";
                        var telefone = "(13) 3377-7666";
        
                        geocoder.geocode( { 'address': endereco}, function(results, status) {
        
                          if (status == google.maps.GeocoderStatus.OK) {
                            posicao = {lat: results[0].geometry.location.lat(), lng: results[0].geometry.location.lng()};
                            criaMapa();
                          }
                        });    
        
                        function criaMapa(){
                            var map = new google.maps.Map(document.getElementById('map'), {
                                zoom: 13,
                                center: posicao
                            });
        
                            var marker = new google.maps.Marker({
                                position: posicao,
                                map: map,
                                icon: marcador
                            });
        
                            var informacao = "<div id=info><h3>"+nome+"</h3><br><b>Servico: </b>"+servico+"<br><b>Telefone: </b>"+telefone+"<br><b>Endereço: </b>"+endereco+" </div>";
        
                            var infowindow = new google.maps.InfoWindow({
                              content: informacao
                            });
        
                            marker.addListener('click', function() {
                                infowindow.open(map, marker);                     
                            });
        
                        }
                    }
                <script async defer src="https://maps.googleapis.com/maps/api/js?key=AIzaSyAeOjMtwy0vXBK5MlFaU4wxf8qRV_ys7Gk&callback=initMap">

    |]
    toWidget [lucius|
        #map {
            height: 550px;
            width: 100%;
        }
        #busca{
            position: absolute;
            top: 550px;
        }
    |]
    

