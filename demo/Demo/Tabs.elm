module Demo.Tabs exposing (..)

import Platform.Cmd exposing (Cmd, none)
import Html exposing (..)
import Html.Attributes exposing (..)

import Material.Tabs as Tabs
import Material
import Material.Options as Options

import Demo.Page as Page
import Demo.Code as Code
import Markdown as Markdown


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
      ({ model | tab = idx }, Cmd.none)

    Mdl action' ->
      Material.update Mdl action' model


-- VIEW


view : Model -> Html Msg
view model  =
  [ Tabs.render Mdl [0] model.mdl
      [ Tabs.ripple
      , Tabs.onSelectTab SelectTab
      , Tabs.selectTab model.tab
      ]
      [ Tabs.tab
          { label =
              Tabs.label
                []
                [text "Example"]

          , content =
              Tabs.content
                []
                [ Code.code """
                            import Material.Tabs as Tabs

                            tabs : Model -> Html Msg
                            tabs model =
                              Tabs.render Mdl [0] model.mdl
                                  [ Tabs.ripple
                                  , Tabs.onSelectTab SelectTab
                                  , Tabs.selectTab model.tab
                                  ]
                                  [ Tabs.tab
                                      { label = Tabs.label [] [text "Tab One"]
                                      , content = Tabs.content [] [text "Tab One content"]
                                      }

                                  , Tabs.tab
                                      { label = Tabs.label [] [text "Tab Two"]
                                      , content = Tabs.content [] [text "Tab Two content"]
                                      }
                                  ]

                            """
                ]
          }
      , Tabs.tab
        { label =
            Tabs.label
              []
              [text "About tabs"]

        , content =
            Tabs.content
              [ Options.css "padding-bottom" "85px" ]
              [ p []
                  [ text """The Material Design Lite (MDL) tab component is a user interface element that allows different content blocks to share the same screen space in a mutually exclusive manner. Tabs are always presented in sets of two or more, and they make it easy to explore and switch among different views or functional aspects of an app, or to browse categorized data sets individually. Tabs serve as "headings" for their respective content; the active tab — the one whose content is currently displayed — is always visually distinguished from the others so the user knows which heading the current content belongs to."""
                  ]
              , p []
                [text """Tabs are an established but non-standardized feature in user interfaces, and allow users to view different, but often related, blocks of content (often called panels). Tabs save screen real estate and provide intuitive and logical access to data while reducing navigation and associated user confusion. Their design and use is an important factor in the overall user experience. See the tab component's Material Design specifications page for details."""]
              ]
        }


      ]
  ] |> Page.body2 "Tabs" srcUrl intro references


intro : Html m
intro =
  Page.fromMDL "https://getmdl.io/components/index.html#layout-section/tabs" """
> The Material Design Lite (MDL) tab component is a user interface element that
> allows different content blocks to share the same screen space in a mutually
> exclusive manner. Tabs are always presented in sets of two or more, and they
> make it easy to explore and switch among different views or functional aspects
> of an app, or to browse categorized data sets individually. Tabs serve as
> "headings" for their respective content; the active tab — the one whose content
> is currently displayed — is always visually distinguished from the others so the
> user knows which heading the current content belongs to.
>
> Tabs are an established but non-standardized feature in user interfaces, and
> allow users to view different, but often related, blocks of content (often
> called panels). Tabs save screen real estate and provide intuitive and logical
> access to data while reducing navigation and associated user confusion. Their
> design and use is an important factor in the overall user experience. See the
> tab component's Material Design specifications page for details.
"""


srcUrl : String
srcUrl =
  "https://github.com/debois/elm-mdl/blob/master/demo/Demo/Tabs.elm"


references : List (String, String)
references =
  [ Page.package "http://package.elm-lang.org/packages/debois/elm-mdl/latest/Material-Tabs"
  , Page.mds "https://material.google.com/components/tabs.html"
  , Page.mdl "https://getmdl.io/components/index.html#layout-section/tabs"
  ]
