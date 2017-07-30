module Demo.Tables exposing (..)

import Html exposing (Html, text)
import Set exposing (Set)
import Material
import Material.Options as Options exposing (when, nop)
import Material.Table as Table
import Material.Options exposing (css)
import Material.Toggles as Toggles
import Demo.Code as Code
import Demo.Page as Page


-- TABLE DATA


type alias Data =
    { material : String
    , quantity : String
    , unitPrice : String
    }


data : List Data
data =
    [ { material = "Acrylic (Transparent)"
      , quantity = "25"
      , unitPrice = "$2.90"
      }
    , { material = "Plywood (Birch)"
      , quantity = "50"
      , unitPrice = "$1.25"
      }
    , { material = "Laminate (Gold on Blue)"
      , quantity = "10"
      , unitPrice = "$2.35"
      }
    ]



{- Unique key for a given data item.  -}


key : Data -> String
key =
    .material


preamble : String
preamble =
    """
import Material.Table as Table

type alias Data =
  { material : String
  , quantity : String
  , unitPrice : String
  }

data : List Data
data =
  [ { material = "Acrylic (Transparent)"   , quantity = "25" , unitPrice = "$2.90" }
  , { material = "Plywood (Birch)"         , quantity = "50" , unitPrice = "$1.25" }
  , { material = "Laminate (Gold on Blue)" , quantity = "10" , unitPrice = "$2.35" }
  ]
  """



-- MODEL


type alias Model =
    { mdl : Material.Model
    , order : Maybe Table.Order
    , selected : Set String
    }


model : Model
model =
    { mdl = Material.model
    , order = Just Table.Ascending
    , selected = Set.empty
    }



-- ACTION, UPDATE


type Msg
    = Mdl (Material.Msg Msg)
    | ToggleAll
    | Toggle String
    | Reorder



{- Rotate table ordering : Ascending -> Descending -> No sorting -> ... -}


rotate : Maybe Table.Order -> Maybe Table.Order
rotate order =
    case order of
        Just (Table.Ascending) ->
            Just Table.Descending

        Just (Table.Descending) ->
            Nothing

        Nothing ->
            Just Table.Ascending



{- Toggle whether or not a set `set` contains an element `x`. -}


toggle : comparable -> Set comparable -> Set comparable
toggle x set =
    if Set.member x set then
        Set.remove x set
    else
        Set.insert x set



{- True iff all rows are currently selected. -}


allSelected : Model -> Bool
allSelected model =
    Set.size model.selected == List.length data


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reorder ->
            { model | order = rotate model.order } ! []

        ToggleAll ->
            -- Click on master checkbox
            { model
                | selected =
                    if allSelected model then
                        Set.empty
                    else
                        List.map key data |> Set.fromList
            }
                ! []

        Toggle idx ->
            -- Click on specific checkbox `idx`
            { model | selected = toggle idx model.selected } ! []

        Mdl msg_ ->
            Material.update Mdl msg_ model



-- VIEW


reverse : comparable -> comparable -> Order
reverse x y =
    case compare x y of
        LT ->
            GT

        GT ->
            LT

        EQ ->
            EQ


basic : Model -> ( String, Html Msg, String )
basic model =
    let
        table =
            Table.table []
                [ Table.thead []
                    [ Table.tr []
                        [ Table.th [] [ text "Material" ]
                        , Table.th [] [ text "Quantity" ]
                        , Table.th [] [ text "Unit Price" ]
                        ]
                    ]
                , Table.tbody []
                    (data
                        |> List.map
                            (\item ->
                                Table.tr []
                                    [ Table.td [] [ text item.material ]
                                    , Table.td [ Table.numeric ] [ text item.quantity ]
                                    , Table.td [ Table.numeric ] [ text item.unitPrice ]
                                    ]
                            )
                    )
                ]

        code =
            """
        Table.table []
        [ Table.thead []
          [ Table.tr []
            [ Table.th [] [ text "Material" ]
            , Table.th [ ] [ text "Quantity" ]
            , Table.th [ ] [ text "Unit Price" ]
            ]
          ]
        , Table.tbody []
            (data |> List.map (\\item ->
               Table.tr []
                 [ Table.td [] [ text item.material ]
                 , Table.td [ Table.numeric ] [ text item.quantity ]
                 , Table.td [ Table.numeric ] [ text item.unitPrice ]
                 ]
               )
            )
        ]
      """
    in
        ( "Static table", table, code )


selectable : Model -> ( String, Html Msg, String )
selectable model =
    let
        table =
            Table.table []
                [ Table.thead []
                    [ Table.tr []
                        [ Table.th []
                            [ Toggles.checkbox Mdl
                                [ -1 ]
                                model.mdl
                                [ Options.onToggle ToggleAll
                                , Toggles.value (allSelected model)
                                ]
                                []
                            ]
                        , Table.th [] [ text "Material" ]
                        , Table.th [ Table.numeric ] [ text "Quantity" ]
                        , Table.th [ Table.numeric ] [ text "Unit Price" ]
                        ]
                    ]
                , Table.tbody []
                    (data
                        |> List.indexedMap
                            (\idx item ->
                                Table.tr
                                    [ Table.selected |> when (Set.member (key item) model.selected) ]
                                    [ Table.td []
                                        [ Toggles.checkbox Mdl
                                            [ idx ]
                                            model.mdl
                                            [ Options.onToggle (Toggle <| key item)
                                            , Toggles.value <| Set.member (key item) model.selected
                                            ]
                                            []
                                        ]
                                    , Table.td [] [ text item.material ]
                                    , Table.td [ Table.numeric ] [ text item.quantity ]
                                    , Table.td [ Table.numeric ] [ text item.unitPrice ]
                                    ]
                            )
                    )
                ]

        code =
            """
    view : Model -> Html Msg
    view model =
      Table.table []
        [ Table.thead []
          [ Table.tr []
            [ Table.th []
                [ Toggles.checkbox Mdl [-1] model.mdl
                  [ Options.onToggle ToggleAll
                  , Toggles.value (allSelected model)
                  ] []
                ]
            , Table.th [] [ text "Material" ]
            , Table.th [ Table.numeric ] [ text "Quantity" ]
            , Table.th [ Table.numeric ] [ text "Unit Price" ]
            ]
          ]
        , Table.tbody []
            ( data
              |> List.indexedMap (\\idx item ->
                   Table.tr
                     [ Table.selected `when` Set.member (key item) model.selected ]
                     [ Table.td []
                       [ Toggles.checkbox Mdl [idx] model.mdl
                         [ Options.onToggle (Toggle <| key item)
                         , Toggles.value <| Set.member (key item) model.selected
                         ] []
                       ]
                     , Table.td [] [ text item.material ]
                     , Table.td [ Table.numeric ] [ text item.quantity ]
                     , Table.td [ Table.numeric ] [ text item.unitPrice ]
                     ]
                 )
            )
        ]


    type alias Model =
      { selected : Set String
      , ...
      }


    update : Msg -> Model -> (Model, Cmd Msg)
    update msg model =
      case msg of
        ...
        ToggleAll ->
          { model
            | selected =
                if allSelected model then
                  Set.empty
                else
                  List.map key data |> Set.fromList
          } ! []

        Toggle k ->
          { model
            | selected =
                if Set.member k model.selected then
                  Set.remove k model.selected
                else
                  Set.insert k model.selected
          } ! []


      allSelected : Model -> Bool
      allSelected model =
        Set.size model.selected == List.length data


      key : Data -> String
      key =
        .material
      """
    in
        ( "Selectable rows", table, code )


sortable : Model -> ( String, Html Msg, String )
sortable model =
    let
        table =
            let
                sort =
                    case model.order of
                        Just (Table.Ascending) ->
                            List.sortBy .material

                        Just (Table.Descending) ->
                            List.sortWith (\x y -> reverse (.material x) (.material y))

                        Nothing ->
                            identity
            in
                Table.table []
                    [ Table.thead []
                        [ Table.tr []
                            [ Table.th
                                [ model.order
                                    |> Maybe.map Table.sorted
                                    |> Maybe.withDefault nop
                                , Options.onClick Reorder
                                ]
                                [ text "Material" ]
                            , Table.th [ Table.numeric ] [ text "Quantity" ]
                            , Table.th [ Table.numeric ] [ text "Unit Price" ]
                            ]
                        ]
                    , Table.tbody []
                        (sort data
                            |> List.indexedMap
                                (\idx item ->
                                    Table.tr []
                                        [ Table.td [] [ text item.material ]
                                        , Table.td [ Table.numeric ] [ text item.quantity ]
                                        , Table.td [ Table.numeric ] [ text item.unitPrice ]
                                        ]
                                )
                        )
                    ]

        code =
            """
        Table.table []
          [ Table.thead []
            [ Table.tr []
              [ Table.th
                  [ """
                ++ (case model.order of
                        Nothing ->
                            ""

                        Just x ->
                            "Table." ++ toString x ++ ", "
                   )
                ++ """Options.onClick Reorder ]
                  [ text "Material" ]
              , Table.th [ Table.numeric ] [ text "Quantity" ]
              , Table.th [ Table.numeric ] [ text "Unit Price" ]
              ]
            ]
          , Table.tbody []
              ( """
                ++ (case model.order of
                        Nothing ->
                            ""

                        Just (Table.Ascending) ->
                            "mySort "

                        Just (Table.Descending) ->
                            "mySortDescending "
                   )
                ++ """data
                |> List.map (\\item ->
                     Table.tr []
                       [ Table.td [] [ text item.material ]
                       , Table.td [ Table.numeric ] [ text item.quantity ]
                       , Table.td [ Table.numeric ] [ text item.unitPrice ]
                       ]
                   )
              )
          ]"""
    in
        ( "Sortable rows", table, code )


tables : Model -> List (Html Msg)
tables model =
    [ basic model
    , sortable model
    , selectable model
    ]
        |> List.concatMap
            (\( title, html, code ) ->
                [ Html.h4 [] [ text title ]
                , Options.div
                    [ css "display" "flex"
                    , css "flex-flow" "row wrap"
                    , css "align-items" "flex-start"
                    ]
                    [ Options.div [ css "margin" "0 24px 24px 0", css "width" "448px" ] [ html ]
                    , Code.code [ css "flex-grow" "1", css "margin" "0 0 24px 0" ] code
                    ]
                ]
            )


view : Model -> Html Msg
view model =
    Page.body1_ "Tables" srcUrl intro references (tables model) [ Html.h4 [] [ text "Import & data" ], Code.code [] preamble ]


intro : Html Msg
intro =
    Page.fromMDL "https://www.getmdl.io/components/index.html#tables-section" """
> The Material Design Lite (MDL) data-table component is an enhanced version of
> the standard HTML &lt;table&gt;. A data-table consists of rows and columns of
> well-formatted data, presented with appropriate user interaction
> capabilities.

> Tables are a ubiquitous feature of most user interfaces, regardless of a
> site's content or function. Their design and use is therefore an important
> factor in the overall user experience. See the data-table component's
> Material Design specifications page for details.

> The available row/column/cell types in a data-table are mostly
> self-formatting; that is, once the data-table is defined, the individual
> cells require very little specific attention. For example, the rows exhibit
> shading behavior on mouseover and selection, numeric values are automatically
> formatted by default, and the addition of a single class makes the table rows
> individually or collectively selectable. This makes the data-table component
> convenient and easy to code for the developer, as well as attractive and
> intuitive for the user.
"""


srcUrl : String
srcUrl =
    "https://github.com/debois/elm-mdl/blob/master/demo/Demo/Tables.elm"


references : List ( String, String )
references =
    [ Page.package "http://package.elm-lang.org/packages/debois/elm-mdl/latest/Material-Table"
    , Page.mds "https://www.google.com/design/spec/components/data-tables.html"
    , Page.mdl "https://getmdl.io/components/index.html#tables-section"
    ]
