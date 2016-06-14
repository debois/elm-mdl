module Demo.ProgressBar exposing (..)

import Platform.Cmd exposing (Cmd, none)
import Html exposing (..)

import Material.ProgressBar as ProgressBar
import Material

import Material.Grid as Grid
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


type Msg
  = TemplateMsg
  | Mdl Material.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    TemplateMsg ->
      (model, Cmd.none)

    Mdl action' ->
      Material.update Mdl action' model


-- VIEW


view : Model -> Html Msg
view model  =
  Page.body2 "ProgressBar" srcUrl intro references
    [
     (Grid.grid []
        [ Grid.cell
            [ Grid.size Grid.All 4]
            [ ProgressBar.render Mdl [0] model.mdl
                [ProgressBar.progress 35]
                []
            , p [] [text "Default progress bar"]
            ]
        , Grid.cell
            [ Grid.size Grid.All 4]
            [ ProgressBar.render Mdl [1] model.mdl
                [ProgressBar.indeterminate] []
            , p [] [text "Indeterminate"]
            ]

        , Grid.cell
            [ Grid.size Grid.All 4]
            [ ProgressBar.render Mdl [1] model.mdl
                [ ProgressBar.buffer 78
                , ProgressBar.progress 44]
                []
            , p [] [text "Buffering "]
            ]
        ])
    ]
  --|> Page.body2 "TEMPLATE" srcUrl intro references


intro : Html m
intro =
  Page.fromMDL "https://www.getmdl.io/components/index.html#TEMPLATE-section" """
> ...
"""


srcUrl : String
srcUrl =
  "https://github.com/debois/elm-mdl/blob/master/demo/Demo/TEMPLATE.elm"


references : List (String, String)
references =
  [ Page.package "http://package.elm-lang.org/packages/debois/elm-mdl/latest/Material-TEMPLATE"
  , Page.mds "https://www.google.com/design/spec/components/TEMPLATE.html"
  , Page.mdl "https://www.getmdl.io/components/index.html#TEMPLATE"
  ]
