port module Main exposing (..)

import Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (..)
import Task


port remember : ( String, String ) -> Cmd msg


port forget : String -> Cmd msg


main =
    programWithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


init : Decode.Value -> ( Model, Cmd Msg )
init flags =
    let
        food =
            Decode.decodeValue (Decode.field "food" (Decode.list foodDecoder)) flags
    in
    case food of
        Ok list ->
            ( Model list [] "" True, Cmd.none )

        Err _ ->
            ( Model [] [] "" True, Cmd.none )


type alias Model =
    { foods : List Food
    , ingredients : List Ingredient
    , input : String
    , sidebarOpen : Bool
    }


type alias Food =
    { name : String
    , carbs : Float
    }


type alias Ingredient =
    ( Food, String, Maybe Float )


type Msg
    = Noop
    | AddIngredient Ingredient
    | UpdateInput String
    | ToggleSidebar
    | UpdateIngredient Int String
    | RemoveIngredient Int


update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        UpdateInput str ->
            ( { model | input = str }, Cmd.none )

        AddIngredient ingr ->
            ( { model | input = "", ingredients = ingr :: model.ingredients }, Task.attempt (always Noop) (Dom.focus "search") )

        ToggleSidebar ->
            ( { model | sidebarOpen = not model.sidebarOpen }, Cmd.none )

        UpdateIngredient index str ->
            let
                update i ( food, inp, weight ) =
                    if i == index then
                        ( food, str, Result.toMaybe (String.toFloat str) )
                    else
                        ( food, inp, weight )

                newIngredients =
                    List.indexedMap update model.ingredients
            in
            ( { model | ingredients = newIngredients }, Cmd.none )

        RemoveIngredient index ->
            let
                removed =
                    List.take index model.ingredients ++ List.drop (index + 1) model.ingredients
            in
            ( { model | ingredients = removed }, Cmd.none )


foodDecoder =
    Decode.map2
        Food
        (Decode.field "name" Decode.string)
        (Decode.field "carbs" Decode.float)


view model =
    Html.div
        []
        [ Html.button
            [ Html.Attributes.class "btn btn-success btn-lg btn-block"
            , Html.Events.onClick ToggleSidebar
            ]
            [ Html.text "Lägg till ingrediens"
            ]
        , ingredientTable model.ingredients
        , sidebar model
        ]


sidebar : Model -> Html Msg
sidebar { sidebarOpen, input, foods } =
    let
        showFood ({ name } as food) =
            Html.li
                [ Html.Attributes.class "list-group-item"
                , Html.Events.onClick (AddIngredient ( food, "", Nothing ))
                ]
                [ Html.text name
                ]
    in
    Html.div
        []
        [ Html.div
            [ Html.Attributes.style
                [ ( "position", "fixed" )
                , ( "top", "0" )
                , ( "left", "0" )
                , ( "right", "0" )
                , ( "bottom"
                  , if sidebarOpen then
                        "0"
                    else
                        "auto"
                  )
                ]
            , Html.Events.onClick ToggleSidebar
            ]
            []
        , Html.div
            [ Html.Attributes.style
                [ ( "position", "fixed" )
                , ( "top", "0" )
                , ( "right", "0" )
                , ( "bottom", "0" )
                , ( "width", "200px" )
                , ( "margin-bottom", "0" )
                , ( "overflow-y", "scroll" )
                , ( "transform"
                  , if sidebarOpen then
                        "none"
                    else
                        "translateX(200px)"
                  )
                ]
            , Html.Attributes.class "panel panel-default"
            ]
            [ Html.input
                [ Html.Attributes.class "form-control"
                , Html.Attributes.id "search"
                , Html.Events.onInput UpdateInput
                , Html.Attributes.placeholder "Skriv för att söka"
                , Html.Attributes.value input
                ]
                []
            , Html.ul
                [ Html.Attributes.class "list-group"
                ]
                (if String.length input > 1 then
                    List.map showFood (List.filter (findFood input) foods)
                 else
                    []
                )
            ]
        ]


findFood : String -> Food -> Bool
findFood str { name } =
    String.contains (String.toLower str) (String.toLower name)


ingredientTable : List Ingredient -> Html Msg
ingredientTable is =
    let
        calc ( { carbs }, _, value ) =
            Maybe.map (\weight -> carbs * weight / 100) value

        carbs =
            Maybe.map List.sum (traverse calc is)
                |> Maybe.map (\num -> toFloat (round (num * 10)) / 10)
                |> Maybe.map (toString >> flip String.append "g")
                |> Maybe.withDefault "Nepp"
    in
    Html.table
        [ Html.Attributes.class "table"
        ]
        [ Html.thead
            []
            [ Html.tr
                []
                [ Html.th
                    []
                    [ Html.text "Ingredienser"
                    ]
                , Html.th
                    [ Html.Attributes.style
                        [ ( "width", "100px" )
                        ]
                    ]
                    [ Html.text "Vikt (g)"
                    ]
                ]
            ]
        , Html.tbody
            []
            (List.indexedMap ingredientTableRow is)
        , Html.tfoot
            []
            [ Html.tr
                []
                [ Html.th
                    []
                    [ Html.text "Kolhydrater"
                    ]
                , Html.th
                    []
                    [ Html.text carbs
                    ]
                ]
            ]
        ]


ingredientTableRow : Int -> Ingredient -> Html Msg
ingredientTableRow index ( { name, carbs }, input, value ) =
    Html.tr
        []
        [ Html.td
            [ Html.Events.onClick (RemoveIngredient index)
            ]
            [ Html.text name
            ]
        , Html.td
            []
            [ Html.div
                [ Html.Attributes.class (Maybe.map (always "") value |> Maybe.withDefault "has-error")
                ]
                [ Html.input
                    [ Html.Attributes.type_ "number"
                    , Html.Attributes.class "form-control"
                    , Html.Attributes.value input
                    , Html.Events.onInput (UpdateIngredient index)
                    ]
                    []
                ]
            ]
        ]


traverse : (a -> Maybe b) -> List a -> Maybe (List b)
traverse f =
    let
        step e acc =
            case f e of
                Nothing ->
                    Nothing

                Just x ->
                    Maybe.map ((::) x) acc
    in
    List.foldr step (Just [])
