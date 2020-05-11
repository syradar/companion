module Main exposing (..)

import Browser
import Browser.Dom
import Html exposing (Html, button, div, h1, h2, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Task



---- MODEL ----


type alias Character =
    { name : String
    , initiative : Int
    , id : Int
    }


type alias Model =
    { characters : List Character
    , round : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { characters =
            [ { name = "Goblin"
              , initiative = 12
              , id = 1
              }
            , { name = "Scanlan"
              , initiative = 10
              , id = 0
              }
            ]
      , round = 1
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | AddCharacter
    | ChangeCharacterName Int String
    | ChangeCharacterInitiative Int String
    | DeleteCharacter Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        AddCharacter ->
            ( { model
                | characters =
                    { name = "Participant #" ++ String.fromInt (List.length model.characters + 1)
                    , initiative = List.length model.characters + 1
                    , id = List.length model.characters + 1
                    }
                        :: model.characters
                        |> sortCharacters
              }
            , Cmd.none
            )

        ChangeCharacterName id newName ->
            let
                updateCharacterName c =
                    if c.id == id then
                        { c | name = newName }

                    else
                        c
            in
            ( { model
                | characters = List.map updateCharacterName model.characters |> sortCharacters
              }
            , Cmd.none
            )

        ChangeCharacterInitiative id newInitiative ->
            let
                updateCharacterInitiative c =
                    if c.id == id then
                        { c | initiative = Maybe.withDefault 0 (String.toInt newInitiative) }

                    else
                        c

                focus =
                    Browser.Dom.focus ("character-" ++ String.fromInt id)
            in
            ( { model
                | characters = List.map updateCharacterInitiative model.characters |> sortCharacters
              }
            , Task.attempt (\_ -> NoOp) focus
            )

        DeleteCharacter id ->
            ( { model | characters = List.filter (\c -> c.id /= id) model.characters }
            , Cmd.none
            )


sortCharacters : List Character -> List Character
sortCharacters characters =
    characters
        |> List.sortBy
            .initiative
        |> List.reverse



---- RENDER CHARACTER ----


renderCharacter : Character -> Html Msg
renderCharacter character =
    div [ class "card" ]
        [ input [ type_ "text", value character.name, onInput (ChangeCharacterName character.id) ] []
        , input [ id ("character-" ++ String.fromInt character.id), type_ "number", value (String.fromInt character.initiative), onInput (ChangeCharacterInitiative character.id) ] []
        , div [] [ text (String.fromInt character.initiative) ]
        , button [ onClick (DeleteCharacter character.id) ] [ text "X" ]
        ]



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "13th Age Companion" ]
        , div [ class "cards" ]
            (model.characters
                |> List.map renderCharacter
            )
        , button [ onClick AddCharacter ] [ text "Add Participant" ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
