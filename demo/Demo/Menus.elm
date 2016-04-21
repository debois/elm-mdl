module Demo.Menus where

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Effects

import Material.Menu as Menu exposing (..)
import Material.Grid as Grid
import Material.Style exposing (Style)

import Demo.Page as Page


-- MODEL


type alias Index = Int


type alias View =
  Signal.Address Menu.Action -> Menu.Model -> List Style -> List Html -> Html


type alias View' =
  Signal.Address Menu.Action -> Menu.Model -> Html


view' : View -> List Style -> Html -> Signal.Address Menu.Action -> Menu.Model -> Html
view' view coloring elem addr model =
  view addr model coloring [elem]


menus : List (Index, (Style, String))
menus =
  [ (Menu.bottomLeft, "Lower left")
  , (Menu.bottomRight, "Lower right")
  , (Menu.topLeft, "Top left")
  , (Menu.topRight, "Top right")
  ]
  |> List.indexedMap (\i r -> (i, r))


model : Model
model =
  { menus = menus
         |> List.map (\(i, (align,_)) -> (i, Menu.model True align))
         |> Dict.fromList
  }


-- ACTION, UPDATE


type Action
  = Action Index Menu.Action


type alias Model =
  { menus : Dict.Dict Index Menu.Model
  }


update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
  case action of
    Action idx action ->
      Dict.get idx model.menus
      |> Maybe.map (\m0 ->
        let
          (m1, e) = Menu.update action m0
        in
          ({ model | menus = Dict.insert idx  m1 model.menus }, Effects.map (Action idx) e)
      )
      |> Maybe.withDefault (model, Effects.none)


-- VIEW


view : Signal.Address Action -> Model -> Html
view addr model =

  menus
  |> List.map (\( idx, (menu, description) ) ->
       let
         model' = model.menus
                |> Dict.get idx
                |> Maybe.withDefault (Menu.model True Menu.unaligned)
         items  =
           [ Menu.item False True  (text "Some Action")
           , Menu.item True  True  (text "Another Action")
           , Menu.item False False (text "Disabled Action")
           , Menu.item False True  (text "Yet Another Action")
           ]
       in
         Grid.cell
         [ Grid.size Grid.All 6
         ]
         [ container addr description idx model' items
         ]
     )
  |> Grid.grid []
  |> flip (::) []
  |> Page.body2 "Menus" srcUrl intro references


container : Signal.Address Action -> String -> Index -> Menu.Model -> List Menu.Item -> Html
container addr description idx model' items =

  let
    bar rightAlign =
      let
        align =
          if rightAlign then ("right", "16px") else ("left", "16px")
      in
        div
          [ class "bar"
          , style
            [ ("box-sizing", "border-box")
            , ("background", "#3F51B5")
            , ("color", "white")
            , ("width", "100%")
            , ("padding", "16px")
            , ("position", "relative")
            , ("height", "64px")
            ]
          ]
          [ div
              [ class "wrapper"
              , style
                [ ("position", "absolute")
                , align
                , ("box-sizing", "border-box")
                ]
              ]
              ( Menu.view (Signal.forwardTo addr (Action idx)) model' items
              )
          ]

    background =
      div
      [ class "background"
      , style
        [ ("height", "148px")
        , ("background", "white")
        , ("width", "100%")
        ]
      ]
      [
      ]

  in

    div
    [ class "section"
    ]
    [ div
        [ class "container mdl-shadow--2dp"
        , style
          [ ("position", "relative")
          , ("width", "200px")
          , ("margin", "0 auto")
          , ("margin-bottom", "40px")
          ]
        ]
        ( if idx > 1 then
              [ background
              , bar (idx % 2 == 1)
              ]
            else
              [ bar (idx % 2 == 1)
              , background
              ]
        )

    , div
        [ style
          [ ("margin", "0 auto")
          , ("width", "200px")
          , ("text-align", "center")
          , ("height", "48px")
          , ("line-height", "48px")
          , ("margin-bottom", "40px")
          ]
        ]
        [ text description
        ]
    ]


intro : Html
intro =
  Page.fromMDL "https://www.getmdl.io/components/#menus-section" """

> The Material Design Lite (MDL) menu component is a user interface element
> that allows users to select one of a number of options. The selection
> typically results in an action initiation, a setting change, or other
> observable effect. Menu options are always presented in sets of two or more,
> and options may be programmatically enabled or disabled as required. The menu
> appears when the user is asked to choose among a series of options, and is
> usually dismissed after the choice is made.
>
> Menus are an established but non-standardized feature in user interfaces, and
> allow users to make choices that direct the activity, progress, or
> characteristics of software. Their design and use is an important factor in
> the overall user experience. See the menu component's <a href="http://www.google.com/design/spec/components/menus.html">Material Design
> specifications page</a> for details.

"""

srcUrl : String
srcUrl =
  "https://github.com/debois/elm-mdl/blob/master/demo/Demo/Menus.elm"

references : List (String, String)
references =
  [ Page.package "http://package.elm-lang.org/packages/debois/elm-mdl/latest/Material-menu"
  , Page.mds "https://www.google.com/design/spec/components/menus.html"
  , Page.mdl "https://www.getmdl.io/components/#menus-section"
  ]

