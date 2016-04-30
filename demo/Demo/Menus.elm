module Demo.Menus where

import Html exposing (Html, text)
import Effects exposing (Effects)

import Material
import Material.Color as Color
import Material.Elevation as Elevation
import Material.Grid as Grid
import Material.Menu as Menu exposing (..)
import Material.Style as Style exposing (cs, css, css', div)
import Material.Helpers exposing (map1st)

import Demo.Page as Page


-- MODEL


type alias Mdl =
  Material.Model


type alias Model =
  { mdl : Material.Model
  }


model : Model
model =
  { mdl = Material.model
  }


-- ACTION, UPDATE


type Action
  = MenuAction Int Menu.Action
  | MDL (Material.Action Action)


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of

    MenuAction idx action ->
      (model, Effects.none)

    MDL action' ->
      Material.update MDL action' model.mdl
      |> map1st (\m -> { model | mdl = m })


-- VIEW


menus : List (Int, Menu.Instance Material.Model Action)
menus =
  [ Menu.BottomLeft
  , Menu.BottomRight
  , Menu.TopLeft
  , Menu.TopRight
  ] |> List.indexedMap (\idx align ->

    (idx, Menu.instance idx MDL (Menu.model True align) [])

  )


items : List Menu.Item
items =
  [ Menu.item False True  (text "Some Action")
  , Menu.item True  True  (text "Another Action")
  , Menu.item False False (text "Disabled Action")
  , Menu.item False True  (text "Yet Another Action")
  ]


describe : Menu.Model -> String
describe menu =
  case menu.alignment of
    BottomLeft -> "Lower left"
    BottomRight -> "Lower right"
    TopLeft -> "Top left"
    TopRight -> "Top right"
    Unaligned -> "Unaligned"


view : Signal.Address Action -> Model -> Html
view addr model =
  menus
  |> List.map (\( idx, c ) ->
       Grid.cell
       [ Grid.size Grid.All 6
       ]
       [ container addr model idx c items
       ]
     )
  |> Grid.grid []
  |> flip (::) []
  |> Page.body2 "Menus" srcUrl intro references


container :
  Signal.Address Action
  -> Model
  -> Int
  -> Menu.Instance Material.Model Action
  -> List Menu.Item
  -> Html
container addr model idx menu items =

  let
    bar idx rightAlign =
      let
        align =
          if rightAlign then ("right", "16px") else ("left", "16px")
      in
        div
          [ Style.cs "bar"
          , Style.css "box-sizing" "border-box"
          , Style.css "width" "100%"
          , Style.css "padding" "16px"
          , Style.css "height" "64px"
          , Color.background Color.accent
          , Color.text Color.white
          ]
          [ div
              [ cs "wrapper"
              , css "box-sizing" "border-box"
              , css "position" "absolute"
              , css' "right" "16px" rightAlign
              , css' "left" "16px" (not rightAlign)
              ]
              ( menu.view addr model.mdl items
              )
          ]

    background =
      div
      [ cs "background"
      , css "height" "148px"
      , css "width" "100%"
      , Color.background Color.white
      ]
      [
      ]

  in

    div
    [ cs "section"
    ]
    [ div
        [ Elevation.e2
        , css "position" "relative"
        , css "width" "200px"
        , css "margin" "0 auto"
        , css "margin-bottom" "40px"
        ]
        ( if idx > 1 then
              [ background
              , bar idx (idx % 2 == 1)
              ]
            else
              [ bar idx (idx % 2 == 1)
              , background
              ]
        )

    , div
        [ css "margin" "0 auto"
        , css "width" "200px"
        , css "text-align" "center"
        , css "height" "48px"
        , css "line-height" "48px"
        , css "margin-bottom" "40px"
        ]
        [ text <| describe <| menu.get model.mdl
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
