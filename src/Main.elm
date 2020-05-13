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
    , isCombatStarted : Bool
    , isEditingInitiative : Bool
    , round : Int
    , escalationDie : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { characters =
            []
      , round = 0
      , isCombatStarted = False
      , isEditingInitiative = False
      , escalationDie = 0
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
    | RollInitiative
    | EndCombat
    | NextRound


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        AddCharacter ->
            ( { model
                | characters =
                    { name = "Participant #" ++ String.fromInt (List.length model.characters + 1)
                    , initiative = 0
                    , id = List.length model.characters + 1
                    }
                        :: model.characters
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
                | characters = List.map updateCharacterName model.characters
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

                focusId =
                    "character-" ++ String.fromInt id
            in
            ( { model
                | characters = List.map updateCharacterInitiative model.characters
              }
            , Task.attempt (\_ -> NoOp) (Browser.Dom.focus focusId)
            )

        DeleteCharacter id ->
            ( { model | characters = List.filter (\c -> c.id /= id) model.characters }
            , Cmd.none
            )

        RollInitiative ->
            let
                focusId =
                    focusFirstCharacterOrBody model.characters
            in
            ( { model
                | isEditingInitiative = True
              }
            , Task.attempt (\_ -> NoOp) (Browser.Dom.focus focusId)
            )

        EndCombat ->
            ( { model
                | isCombatStarted = False
                , round = 0
                , escalationDie = 0
              }
            , Cmd.none
            )

        NextRound ->
            let
                isFirstRound =
                    not model.isCombatStarted
            in
            ( { model
                | round = model.round + 1
                , escalationDie = clamp 0 5 model.escalationDie + 1
                , characters =
                    if isFirstRound then
                        model.characters |> sortCharacters

                    else
                        model.characters
                , isCombatStarted = True
                , isEditingInitiative = False
              }
            , Cmd.none
            )


sortCharacters : List Character -> List Character
sortCharacters characters =
    characters
        |> List.sortBy
            .initiative
        |> List.reverse



-- Focus Functions --


focusFirstCharacterOrBody : List Character -> String
focusFirstCharacterOrBody characters =
    case List.head characters of
        Just character ->
            "character-" ++ String.fromInt character.id

        Nothing ->
            "body"



---- RENDER CHARACTER ----


renderCharacter : Character -> Html Msg
renderCharacter character =
    div [ class "card" ]
        [ div [] [ text character.name ]
        , div [ class "initiative" ] [ text (String.fromInt character.initiative) ]
        , button [ onClick (DeleteCharacter character.id) ] [ text "X" ]
        ]


renderEditingInitiativeCharacter : Character -> Html Msg
renderEditingInitiativeCharacter character =
    div [ class "card" ]
        [ div [] [ text character.name ]
        , input [ class "initiative", id ("character-" ++ String.fromInt character.id), type_ "number", value (String.fromInt character.initiative), onInput (ChangeCharacterInitiative character.id) ] []
        ]


renderEditingNameCharacter : Character -> Html Msg
renderEditingNameCharacter character =
    div [ class "card" ]
        [ input [ type_ "text", value character.name, onInput (ChangeCharacterName character.id) ] []
        , button [ onClick (DeleteCharacter character.id) ] [ text "X" ]
        ]



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        renderCharactersFunction =
            if model.isCombatStarted then
                renderCharacter

            else if model.isEditingInitiative then
                renderEditingInitiativeCharacter

            else
                renderEditingNameCharacter
    in
    div []
        [ h1 []
            [ text "13th Age Companion" ]
        , if model.isCombatStarted then
            div [ class "info" ]
                [ button
                    [ onClick EndCombat, class "end" ]
                    [ text "End combat" ]
                , if not (List.isEmpty model.characters) then
                    button
                        [ onClick NextRound, class "next" ]
                        [ text ("Start turn " ++ String.fromInt (model.round + 1)) ]

                  else
                    text ""
                ]

          else if not (List.isEmpty model.characters) then
            div [ class "info" ]
                [ if model.isEditingInitiative then
                    button
                        [ onClick NextRound, class "start" ]
                        [ text ("Start turn " ++ String.fromInt (model.round + 1)) ]

                  else
                    button
                        [ onClick RollInitiative, class "start" ]
                        [ text "Roll initiative" ]
                ]

          else
            div []
                [ text "" ]
        , if model.isCombatStarted then
            div [ class "info" ]
                [ h2
                    [ class "round" ]
                    [ text ("ROUND: " ++ String.fromInt model.round) ]
                , h2
                    [ class "escalation" ]
                    [ text ("Escalation Die: " ++ String.fromInt model.escalationDie) ]
                ]

          else
            text ""
        , div [ class "cards" ]
            (model.characters
                |> List.map renderCharactersFunction
            )
        , if not model.isCombatStarted then
            button [ onClick AddCharacter ] [ text "Add Participant" ]

          else
            text ""
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
