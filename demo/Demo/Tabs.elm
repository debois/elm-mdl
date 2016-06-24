module Demo.Tabs exposing (..)

import Platform.Cmd exposing (Cmd, none)
import Html exposing (..)
import Html.Attributes exposing (..)

import Material.Tabs as Tabs
import Material
import Material.Options as Options

import Demo.Page as Page


-- MODEL


type alias Mdl =
  Material.Model


type alias Model =
  { mdl : Material.Model
  , tab : Int
  }


model : Model
model =
  { mdl = Material.model
  , tab = 0
  }


-- ACTION, UPDATE


type Msg
  = SelectTab Int
  | Mdl Material.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    SelectTab idx ->
      let
        _ = Debug.log "SELECTED" idx
      in
        ({ model | tab = idx }, Cmd.none)

    Mdl action' ->
      Material.update Mdl action' model


-- VIEW


view : Model -> Html Msg
view model  =
  [ div
      []
      [ Tabs.render Mdl [0] model.mdl
          [ Tabs.ripple
          , Tabs.onSelectTab SelectTab
          , Tabs.selectTab model.tab
          ]
          [ Tabs.tab
             { link =
                 Tabs.link
                 []
                 [text "About tabs"]

             , panel =
                 Tabs.panel
                   []
                   [ p []
                       [ b [] [text "Tab"]
                       , text " component is pretty cool."
                       ]
                   , p []
                       [ text """The Material Design Lite (MDL) tab component is a user interface element that allows different content blocks to share the same screen space in a mutually exclusive manner. Tabs are always presented in sets of two or more, and they make it easy to explore and switch among different views or functional aspects of an app, or to browse categorized data sets individually. Tabs serve as "headings" for their respective content; the active tab — the one whose content is currently displayed — is always visually distinguished from the others so the user knows which heading the current content belongs to."""
                       ]
                   ]
             }

          , Tabs.tab
             { link =
                 Tabs.link
                   []
                   [text "Example"]

             , panel =
                 Tabs.panel
                   []
                   [
                   ]
             }

          ]
      ]
  , div
      [ style [("margin-top", "60px")]]
      []
  ]
  |> Page.body2 "TEMPLATE" srcUrl intro references


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
