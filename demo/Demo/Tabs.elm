module Demo.Tabs exposing (..)

import Platform.Cmd exposing (Cmd, none)
import Html exposing (..)
import Html.Attributes exposing (..)

import Material.Tabs as Tabs
import Material

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
  [ div
      []
      [ Tabs.render Mdl [0] model.mdl
          [ Tabs.ripple ]
          [ Tabs.tab
             { panel = Tabs.panel [] [text "Tab One Content"]
             , link = Tabs.link [] [text "Tab One"]
             }
          , Tabs.tab
             { panel = Tabs.panel [] [text "Tab One Two"]
             , link = Tabs.link [] [text "Tab Two"]
             }

          ]
          -- [ Tabs.tabBar []
          --     [ Tabs.tabLink [Tabs.active] [text "Tab One"]
          --     , Tabs.tabLink [] [text "Tab Two"]
          --     ]
          -- , Tabs.panel [Tabs.active]
          --   [text "Panel 1"]

          -- , Tabs.panel []
          --   [text "Panel 2"]
          -- ]
      ]
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
