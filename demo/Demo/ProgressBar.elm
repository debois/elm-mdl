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


intro : Html m
intro =
  Page.fromMDL "https://getmdl.io/components/index.html#loading-section/progress" """
> The Material Design Lite (MDL) progress component is a visual indicator of
> background activity in a web page or application. A progress indicator
> consists of a (typically) horizontal bar containing some animation that
> conveys a sense of motion. While some progress devices indicate an approximate
> or specific percentage of completion, the MDL progress component simply
> communicates the fact that an activity is ongoing and is not yet complete.

> Progress indicators are an established but non-standardized feature in user
> interfaces, and provide users with a visual clue to an application's status.
> Their design and use is therefore an important factor in the overall user
> experience. See the progress component's Material Design specifications page for
> details.
"""


srcUrl : String
srcUrl =
  "https://github.com/debois/elm-mdl/blob/master/demo/Demo/ProgressBar.elm"


references : List (String, String)
references =
  [ Page.package "http://package.elm-lang.org/packages/debois/elm-mdl/latest/Material-ProgressBar"
  , Page.mds "https://www.google.com/design/spec/components/progress-activity.html"
  , Page.mdl "https://getmdl.io/components/index.html#loading-section/progress"
  ]
