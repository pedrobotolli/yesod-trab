module Cpf exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

-- MODEL

type alias Model =
  { cpf : String
  }
  
modelInicial : Model
modelInicial =
  Model "" 

-- UPDATE

type Msg
    = CampoCpf String
    | Validar


    
validarCPF : String -> Bool
validarCPF cpf = 
    if (cpf=="") 
      then FALSE
    else if (length(cpf) != 11 || 
                cpf == "00000000000" || 
                cpf == "11111111111" || 
                cpf == "22222222222" || 
                cpf == "33333333333" || 
                cpf == "44444444444" || 
                cpf == "55555555555" || 
                cpf == "66666666666" || 
                cpf == "77777777777" || 
                cpf == "88888888888" || 
                cpf == "99999999999")
                then FALSE
                else 
                for (i=0; i < 9; i ++)       
                  add += parseInt(cpf.charAt(i)) * (10 - i);  
                  var rev = 11 - (add % 11);  
                  if (rev == 10 || rev == 11) then  
                      rev = 0;    
                  if (rev != parseInt(cpf.charAt(9)))  then   
                      return false;       
                add = 0;    
                for (i = 0; i < 10; i ++)        
                  add += parseInt(cpf.charAt(i)) * (11 - i);  
                  rev = 11 - (add % 11);  
                  if (rev == 10 || rev == 11) then
                    rev = 0;    
                  if (rev != parseInt(cpf.charAt(10))) then
                    return false;
                return true;  

updateView : Msg -> Model -> Model
updateView action model =
  case action of
    CampoCpf x ->
      { model | cpf = x }
    
    Validar ->
      { model | res = validarCPF x }

-- VIEW

viewSoma : Model -> Html Msg
viewSoma model =
  div []
    [ input [ type_ "text", step "0.01", style [("color", "blue"),("margin-left","40%"),("margin-top","100px"), ("width","125px"),("height","50px"),("text-align","center")], placeholder "Digite seu CPF", onInput CampoCPF] []
    , br [] []
    , button [onClick Calcular,style [("color", "red"),("margin-left","50px"), ("width","125px"),("height","50px"),("text-align","center")]] [text "OK"]
    , br [] []
    , div[style [("color", "red"),("margin-left","40%"), ("width","125px"),("height","50px"), ("margin-top","50px")]] [text (toString model.res)]
    ] 

main =
  beginnerProgram 
    { model = modelInicial
    , view = viewSoma
    , update = updateView 
    }