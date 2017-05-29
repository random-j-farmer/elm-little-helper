module Pilots exposing (main, mainForView, view, Model, Msg)

import Html exposing (..)
import Html.Attributes  exposing (..)
import Http
import Json.Decode exposing (Decoder, map8, string, list, nullable, int, field)
import Bootstrap.Grid as Grid
import Bootstrap.Form as Form
-- import Bootstrap.Form.Input as Input
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Button as Button
import Bootstrap.Alert as Alert

main : Program Never Model Msg
main =
  mainForView view

mainForView: (Model -> Html Msg) -> Program Never Model Msg
mainForView wrappedView =
  Html.program { init = init, view = wrappedView, update = update, subscriptions = subscriptions }


-- MODEL

type alias Model
  = { pilotNames : String
    , pilotInfos : List PilotInfo
    , pilotInfosError : Maybe String
    }


type alias PilotInfo
  = { characterName : String
    , characterID : Int
    , corporationName : String
    , corporationID : Int
    , allianceName : Maybe String
    , allianceID : Maybe Int
    , factionName : Maybe String
    , recentKills : Int
    }


pilotInfoDecoder : Decoder PilotInfo
pilotInfoDecoder =
  map8 PilotInfo
    (field "name" string)
    (field "id" int)
    (field "corporation_name" string)
    (field "corporation_id" int)
    (field "alliance_name" (nullable string))
    (field "alliance_id" (nullable int))
    (field "faction_name" (nullable string))
    (field "recent_kills" int)


-- UPDATE

type Msg
  = PilotNames String
  | LookupPilotNames
  | LookupPilotResult (Result Http.Error (List PilotInfo))

init : (Model, Cmd Msg)
init =
  (Model "" [] Nothing, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    PilotNames names ->
      ({ model | pilotNames = names }, Cmd.none)

    LookupPilotNames  ->
      ({ model | pilotNames = "" }, lookupPilotNames model.pilotNames)

    LookupPilotResult (Ok infos) ->
      ({ model | pilotInfos = infos, pilotInfosError = Nothing }, Cmd.none)

    LookupPilotResult (Err err) ->
      ({ model | pilotInfos = [], pilotInfosError = Just (toString err) }, Cmd.none)

lookupPilotNames : String -> Cmd Msg
lookupPilotNames names =
   let
    url =
      "http://localhost:3000/rlh/pilots"
    body =
      Http.multipartBody [ Http.stringPart "names" names ]

    request =
      Http.post url body (Json.Decode.list pilotInfoDecoder)
  in
    Http.send LookupPilotResult request

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ Grid.container []
      [ h1 [] [ text "Pilots" ]
      , Grid.row []
        [ Grid.col []
            [ Form.form []
              [ Form.group [ ]
                [ Form.label [ for "pilotNamesArea" ] [ text "Pilot Names" ]
                , Textarea.textarea [ Textarea.id "pilotNamesArea"
                                    , Textarea.rows 5
                                    , Textarea.value model.pilotNames
                                    , Textarea.onInput PilotNames ]
                ]
              ]
              , Button.button [ Button.primary, Button.onClick LookupPilotNames] [ text "Submit" ]
            ]
        , Grid.col [] (prependError model.pilotInfosError (viewPilotInfos model.pilotInfos))
        ]
      ]
    ]

prependError : Maybe String -> List (Html Msg) -> List (Html Msg)
prependError err list =
  case err of
    Nothing ->
      list

    Just x ->
      Alert.danger [ text x ] :: list



viewPilotInfos : List PilotInfo -> List (Html Msg)
viewPilotInfos infos =
  [ if (List.isEmpty infos)
    then
      p [] [ text "No result." ]
    else
      table []
        [ thead []
            [ th [] [ text "Name" ]
            , th [] [ text "Corporation" ]
            , th [] [ text "Alliance" ]
            , th [] [ text "Faction" ]
            , th [] [ text "Recent Kills" ]
            ]
        , tbody []
            (List.map viewPilotInfo infos)
        ]
  ]

viewPilotInfo : PilotInfo -> Html Msg
viewPilotInfo info =
  tr []
    [ td [] [ text info.characterName ]
    , td [] [ text info.corporationName ]
    , td [] [ text (Maybe.withDefault "" info.allianceName) ]
    , td [] [ text (Maybe.withDefault "" info.factionName) ]
    , td [] [ text (toString info.recentKills) ]
    ]
