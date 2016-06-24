module Material.Tabs exposing (..)

{-| From the [Material Design Lite documentation](http://www.getmdl.io/components/#TEMPLATE-section):

> ...

See also the
[Material Design Specification]([https://www.google.com/design/spec/components/TEMPLATE.html).

Refer to [this site](http://debois.github.io/elm-mdl#/template)
for a live demo.

@docs Model, model, Msg, update
@docs view

# Component support

@docs Container, Observer, Instance, instance, fwdTemplate
-}

-- TEMPLATE. Copy this to a file for your component, then update.

import Platform.Cmd exposing (Cmd, none)
import Html exposing (Html)
import Parts exposing (Indexed)
import Material.Options as Options exposing (cs, when)
import Material.Options.Internal as Internal
import Material.Helpers as Helpers
import Material.Ripple as Ripple
import Html.App
import Html.Attributes as Html exposing (class)
import Html.Events as Html
import Dict exposing (Dict)

import Json.Decode as Json


-- MODEL


{-| Component model.
-}
type alias Model =
  { ripples : Dict Int Ripple.Model
  }


{-| Default component model constructor.
-}
defaultModel : Model
defaultModel =
  { ripples = Dict.empty
  }


-- ACTION, UPDATE


{-| Component action.
-}
type Msg
  = NoOp
  | Ripple Int Ripple.Msg



{-| Component update.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
  case action of
    NoOp ->
      (model, none)

    Ripple tabIdx action' ->
      let
        ( ripple', cmd ) =
          Dict.get tabIdx model.ripples
            |> Maybe.withDefault Ripple.model
            |> Ripple.update action'
      in
        ( { model | ripples = Dict.insert tabIdx ripple' model.ripples }, Cmd.map (Ripple tabIdx) cmd )



-- PROPERTIES


type alias Config m =
  { ripple : Bool
  , onSelectTab : Maybe (Int -> m)
  , activeTab : Int
  }


defaultConfig : Config m
defaultConfig =
  { ripple = False
  , onSelectTab = Nothing
  , activeTab = 0
  }


type alias Property m =
  Options.Property (Config m) m


type Content m
  = Content
      { styles : List (Property m)
      , content : List (Html m)
      }


type TabLink m
  = TabLink
      { styles : List (Property m)
      , content : List (Html m)
      }


type Tab m
  = Tab
      { link : TabLink m
      , content : Content m
      }


tab : { content : Content m, link : TabLink m } -> Tab m
tab =
  Tab


content : List (Property m) -> List (Html m) -> Content m
content styles content =
  Content { styles = styles, content = content }


link : List (Property m) -> List (Html m) -> TabLink m
link styles content =
  TabLink { styles = styles, content = content }


ripple : Property m
ripple =
  Options.set (\options -> { options | ripple = True })


{-| Receieve notification when tab `k` is selected.
-}
onSelectTab : (Int -> m) -> Property m
onSelectTab f =
  Options.set (\config -> { config | onSelectTab = Just (f) })


{-| Receieve notification when tab `k` is selected.
-}
selectTab : Int -> Property m
selectTab tab =
  Options.set (\config -> { config | activeTab = tab })


{-| Component view.
-}
view : (Msg -> m) -> Model -> List (Property m) -> List (Tab m) -> Html m
view lift model options tabs =
  let
    summary =
      Options.collect defaultConfig options

    config =
      summary.config

    unwrapPanel tabIdx (Content { styles, content }) =
      Options.styled Html.div
        ([ cs "mdl-tabs__panel"
         , cs "is-active" `when` (tabIdx == config.activeTab)
         ] ++ styles)
        content

    unwrapLink tabIdx (TabLink { styles, content }) =
      Options.styled Html.a
        ([ cs "mdl-tabs__tab"
        , cs "is-active" `when` (tabIdx == config.activeTab)
        , config.onSelectTab
          |> Maybe.map (\t -> Internal.attribute <| Html.onClick (t tabIdx))
          |> Maybe.withDefault Options.nop
        ] ++ styles)
        (if config.ripple then
          List.concat
            [ content
            , [ Ripple.view
                  [ Html.classList
                      [ ( "mdl-tabs__ripple-container", True )
                      , ( "mdl-tabs__ripple-js-effect", True )
                      ]
                  ]
                  (Dict.get tabIdx model.ripples
                    |> Maybe.withDefault Ripple.model
                  )
                  |> Html.App.map (Ripple tabIdx >> lift)
              ]
            ]
         else
          content
        )

    unwrapTab tabIdx (Tab { content, link }) =
      ( unwrapPanel tabIdx content, unwrapLink tabIdx link )

    tabs' =
      List.indexedMap unwrapTab tabs

    ( panels, links' ) =
      List.unzip tabs'

    links =
      Options.styled Html.div
        (cs "mdl-tabs__tab-bar" :: [])
        links'
  in
    Options.apply summary
      Html.div
      [ cs "mdl-tabs"
      , cs "mdl-js-tabs"
      , cs "is-upgraded"
      , cs "mdl-js-ripple-effect" `when` config.ripple
      , cs "mdl-js-ripple-effect--ignore-events" `when` config.ripple
      ]
      []
      (links :: panels)



-- COMPONENT


type alias Container c =
  { c | tabs : Indexed Model }


{-| Component render.
-}
render :
  (Parts.Msg (Container c) -> m)
  -> Parts.Index
  -> Container c
  -> List (Property m)
  -> List (Tab m)
  -> Html m
render =
  Parts.create view update .tabs (\x y -> { y | tabs = x }) defaultModel



{- See src/Material/Layout.mdl for how to add subscriptions. -}
