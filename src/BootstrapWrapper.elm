module BootstrapWrapper exposing (main)

import Html exposing (..)
import Bootstrap.CDN as CDN
import Pilots

main : Program Never Pilots.Model Pilots.Msg
main = Pilots.mainForView view

view : Pilots.Model -> Html Pilots.Msg
view model =
  div []
        [ CDN.stylesheet -- inline stylesheet with bootstrap css
        , Pilots.view model
        ]
