    function initMap() {
                var geocoder = new google.maps.Geocoder();
                var endereco ="Guarujá, São Paulo, Brasil";
                var posicao;
                var marcador = "../static/html/marcador.png";
                var medialat=0;
                var medialong=0;
                console.log("foi porra...");
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
                    console.log("foi MESMO porra...");
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
