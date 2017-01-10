module Demo.Select exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Platform.Cmd exposing (Cmd, none)
import Material
import Material.Grid as Grid
import Material.Options exposing (Style)
import Material.Select as Select
import Material.Select.Item as Item
import Demo.Code as Code
import Demo.Page as Page


-- MODEL


type alias Model =
    { mdl : Material.Model
    , values : Dict Int String
    , indices : Dict Int Int
    }


model : Model
model =
    { mdl = Material.model
    , values = Dict.empty
    , indices = Dict.empty
    }



-- ACTION, UPDATE


type Msg
    = Select Int String
    | SelectByIndex Int Int
    | Mdl (Material.Msg Msg)
    | Focus


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Select index string ->
            { model | values = Dict.insert index string model.values } ! []

        SelectByIndex index selectedIndex ->
            { model | indices = Dict.insert index selectedIndex model.indices } ! []

        Focus ->
            Material.update Mdl (Select.closeAll Mdl model.mdl) model

        Mdl msg ->
            Material.update Mdl msg model



-- VIEW


view : Model -> Html Msg
view model =
    [ Html.h4 [] [ text "Simple example" ]
    , Grid.grid []
        [ Grid.cell column
            [ Select.render Mdl
                [ 0 ]
                model.mdl
                [ Select.label "Dinosaurs"
                , Select.floatingLabel
                , Select.ripple
                , Select.value (Maybe.withDefault "" (Dict.get 0 model.values))
                , Select.onFocus Focus
                ]
                ([ "allosaurus"
                 , "brontosaurus"
                 , "carcharodontosaurus"
                 , "diplodocus"
                 ]
                    |> List.map
                        (\string ->
                            Select.item
                                [ Item.onSelect (Select 0 string)
                                , Item.ripple
                                ]
                                [ text string
                                ]
                        )
                )
            ]
        , Grid.cell column
            [ Code.code [] """
          Select.render Mdl [0] model.mdl
          [ Select.label "Dinosaurs"
          , Select.floatingLabel
          , Select.ripple
          , Select.value model.value
          ]
          ( [ "allosaurus"
            , "brontosaurus"
            , "carcharodontosaurus"
            , "diplodocus"
            ]
            |> List.map (\\string ->
                 Select.item
                 [ Item.onSelect (Select string)
                 , Item.ripple
                 ]
                 [ text string
                 ]
               )
          )
        """
            ]
        ]
    , Html.h4 [] [ text "Example without ripple and floating label" ]
    , Grid.grid []
        [ Grid.cell column
            [ Select.render Mdl
                [ 1 ]
                model.mdl
                [ Select.label "Dinosaurs"
                , Select.value (Maybe.withDefault "" (Dict.get 1 model.values))
                , Select.onFocus Focus
                ]
                ([ "allosaurus"
                 , "brontosaurus"
                 , "carcharodontosaurus"
                 , "diplodocus"
                 ]
                    |> List.map
                        (\string ->
                            Select.item
                                [ Item.onSelect (Select 1 string)
                                ]
                                [ text string
                                ]
                        )
                )
            ]
        , Grid.cell column
            [ Code.code [] """
          Select.render Mdl [0] model.mdl
          [ Select.label "Dinosaurs"
          , Select.value model.value
          ]
          ( [ "allosaurus"
            , "brontosaurus"
            , "carcharodontosaurus"
            , "diplodocus"
            ]
            |> List.map (\\string ->
                 Select.item
                 [ Item.onSelect (Select string)
                 ]
                 [ text string
                 ]
               )
          )
        """
            ]
        ]
    , Html.h4 [] [ text "Example with selected value" ]
    , Grid.grid []
        [ Grid.cell column
            [ let
                values =
                    [ "allosaurus"
                    , "brontosaurus"
                    , "carcharodontosaurus"
                    , "diplodocus"
                    ]

                index =
                    Dict.get 2 model.indices
                        |> Maybe.withDefault 0

                selectedValue =
                    values
                        |> List.drop index
                        |> List.head
                        |> Maybe.withDefault ""
              in
                Select.render Mdl
                    [ 2 ]
                    model.mdl
                    [ Select.label "Dinosaurs"
                    , Select.value selectedValue
                    , Select.index index
                    , Select.onFocus Focus
                    ]
                    (values
                        |> List.indexedMap
                            (\index string ->
                                Select.item
                                    [ Item.onSelect (SelectByIndex 2 index)
                                    ]
                                    [ text string
                                    ]
                            )
                    )
            ]
        , Grid.cell column
            [ Code.code [] """
          let
            values =
              [ "allosaurus"
              , "brontosaurus"
              , "carcharodontosaurus"
              , "diplodocus"
              ]

            selectedValue =
              values
              |> List.drop model.index
              |> List.head
              |> Maybe.withDefault ""
          in
            Select.render Mdl [0] model.mdl
            [ Select.label "Dinosaurs"
            , Select.value selectedValue
            , Select.index model.index
            ]
            ( values
              |> List.indexedMap (\\index string ->
                   Select.item
                   [ Item.onSelect (Select index)
                   ]
                   [ text string
                   ]
                 )
            )
        """
            ]
        ]
    ]
        |> Page.body2 "Select" srcUrl intro references


column : List (Style m)
column =
    [ Grid.size Grid.Desktop 6, Grid.size Grid.Tablet 8 ]


intro : Html m
intro =
    Page.fromMDL "https://www.getmdl.io/components/index.html#TEMPLATE-section" """
> ...
"""


srcUrl : String
srcUrl =
    "https://github.com/debois/elm-mdl/blob/master/demo/Demo/TEMPLATE.elm"


references : List ( String, String )
references =
    [ Page.package "http://package.elm-lang.org/packages/debois/elm-mdl/latest/Material-TEMPLATE"
    , Page.mds "https://www.google.com/design/spec/components/TEMPLATE.html"
    , Page.mdl "https://www.getmdl.io/components/index.html#TEMPLATE"
    ]
