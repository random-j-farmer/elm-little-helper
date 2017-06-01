module Pilots exposing (main, mainForView, view, Model, Msg, Flags)

import Html exposing (..)
import Html.Attributes  exposing (..)
import Http
import Dict
import Json.Decode exposing (Decoder, map8, string, list, nullable, int, field)
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Form as Form
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Button as Button
import Bootstrap.Alert as Alert

main : Program Flags Model Msg
main =
  mainForView view

mainForView: (Model -> Html Msg) -> Program Flags Model Msg
mainForView wrappedView =
  Html.programWithFlags  { init = init, view = wrappedView, update = update, subscriptions = subscriptions }


-- MODEL

type alias Flags
  = { backendUrl : String }

type alias Model
  = { flags : Flags
    , lookupInProgress : Bool
    , pilotNames : String
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

init : Flags -> (Model, Cmd Msg)
init flags =
  (Model flags False "" [] Nothing, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    PilotNames names ->
      ({ model | pilotNames = names }, Cmd.none)

    LookupPilotNames  ->
      ( { model
        | lookupInProgress = True
        , pilotNames = ""}
      , lookupPilotNames model.flags.backendUrl model.pilotNames)

    LookupPilotResult (Ok infos) ->
      ( { model
        | lookupInProgress = False
        , pilotInfos = infos
        , pilotInfosError = Nothing }
      , Cmd.none)

    LookupPilotResult (Err err) ->
      ( { model
          | lookupInProgress = False
          , pilotInfos = []
          , pilotInfosError = Just (toString err) }
      , Cmd.none)

lookupPilotNames : String -> String -> Cmd Msg
lookupPilotNames url names =
  let
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
    [ Grid.containerFluid []
      [ h1 [] [ text "Pilots" ]
      , Grid.row []
        [ Grid.col [Col.xs4]
            [ Form.form []
              [ Form.group [ ]
                [ Form.label [ for "pilotNamesArea" ] [ text "Pilot Names" ]
                , Textarea.textarea [ Textarea.id "pilotNamesArea"
                                    , Textarea.rows 5
                                    , Textarea.value model.pilotNames
                                    , Textarea.onInput PilotNames ]
                ]
              ]
              , Button.button [ Button.primary
                              , Button.disabled model.lookupInProgress
                              , Button.onClick LookupPilotNames ]
                              [ text "Submit" ]
            ]
        , Grid.col [Col.xs8]
            <| Maybe.withDefault
              (if List.isEmpty model.pilotInfos
                then [ Alert.info [ text "No Result." ] ]
                else viewInfos model.pilotInfos)
              (Maybe.map viewError model.pilotInfosError)
        ]
      ]
    ]

viewError : String -> List (Html Msg)
viewError err =
  [ Alert.danger [ text err ] ]


viewInfos: List PilotInfo -> List (Html Msg)
viewInfos infos =
  [ h2 [] [ text "Pilots" ]
  , viewPilotInfos infos
  ]

viewPilotInfos : List PilotInfo -> Html Msg
viewPilotInfos infos =
  table [ style [ ("width", "100%") ] ]
        [ thead []
            [ th [] [ text "Name" ]
            , th [] [ text "Corp/Alliance"]
            , th [] [ text "Faction" ]
            , th [] [ text "Recent Kills" ]
            ]
        , tbody []
            (List.map (viewPilotInfo <| corpCounts infos) infos)
        ]

viewPilotInfo : Dict.Dict String Int -> PilotInfo -> Html Msg
viewPilotInfo corps info =
  tr []
    [ td [] [ zkillLink info ]
    , td [] [ text <| allianceOrCorpWithCount corps info ]
    , td [] [ text (Maybe.withDefault "" info.factionName) ]
    , td [] [ text (toString info.recentKills) ]
    ]

zkillLink : PilotInfo -> Html a
zkillLink info =
  a [ href <| "https://zkillboard.com/character/" ++ toString info.characterID
    , target "_blank" ]
    [ text info.characterName ]

allianceOrCorpWithCount : Dict.Dict String Int -> PilotInfo -> String
allianceOrCorpWithCount corps info =
  let name = allianceOrCorp info
      cnt = Maybe.withDefault 1 <| Dict.get name corps
  in name ++ " (" ++ toString cnt ++ ")"

allianceOrCorp : PilotInfo -> String
allianceOrCorp info =
  case info.allianceName of
    Nothing -> info.corporationName
    Just x -> x

corpCounts : List PilotInfo -> Dict.Dict String Int
corpCounts infos =
  let incKey v = case v of
        Nothing -> Just 1
        Just x -> Just <| x + 1
      corps = List.map allianceOrCorp infos
  in List.foldl (\k d -> Dict.update k incKey d) Dict.empty corps
