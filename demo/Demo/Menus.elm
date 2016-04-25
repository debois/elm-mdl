module Demo.Menus where

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Effects exposing (Effects)

import Material.Color as Color
import Material.Elevation as Elevation
import Material.Grid as Grid
import Material.Menu as Menu exposing (..)
import Material.Style as Style exposing (Style, cs)

import Demo.Page as Page


-- MODEL


type alias Index = Int


menus : List (Index, (Alignment, String))
menus =
  [ (Menu.BottomLeft, "Lower left")
  , (Menu.BottomRight, "Lower right")
  , (Menu.TopLeft, "Top left")
  , (Menu.TopRight, "Top right")
  ] |> List.indexedMap (,)


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


update : Action -> Model -> (Model, Effects Action)
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
                |> Maybe.withDefault (Menu.model True Menu.Unaligned)
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
        Style.div
          [ Style.cs "bar"
          , Color.background Color.accent
          , Color.text Color.white
          , Style.attribute
            ( style
              [ ("box-sizing", "border-box")
              , ("width", "100%")
              , ("padding", "16px")
              , ("position", "relative")
              , ("height", "64px")
              ]
            )
          ]
          [ Style.div
              [ cs "wrapper"
              , Style.attribute
                ( style
                  [ ("position", "absolute")
                  , align
                  , ("box-sizing", "border-box")
                  ]
                )
              ]
              ( Menu.view (Signal.forwardTo addr (Action idx)) model' items
              )
          ]

    background =
      Style.div
      [ cs "background"
      , Style.attribute
        ( style
          [ ("height", "148px")
          , ("background", "white")
          , ("width", "100%")
          ]
        )
      ]
      [
      ]

  in

    Style.div
    [ cs "section"
    ]
    [ Style.div
        [ Elevation.e2
        , Style.attribute
          ( style
            [ ("position", "relative")
            , ("width", "200px")
            , ("margin", "0 auto")
            , ("margin-bottom", "40px")
            ]
          )
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

    , Style.div
        [ Style.attribute
          ( style
            [ ("margin", "0 auto")
            , ("width", "200px")
            , ("text-align", "center")
            , ("height", "48px")
            , ("line-height", "48px")
            , ("margin-bottom", "40px")
            ]
          )
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

