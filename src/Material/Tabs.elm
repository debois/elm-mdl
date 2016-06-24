module Material.Tabs exposing
  (..)

-- TEMPLATE. Copy this to a file for your component, then update.

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


import Platform.Cmd exposing (Cmd, none)
import Html exposing (Html)
import Parts exposing (Indexed)
import Material.Options as Options exposing (cs, when)
import Material.Helpers as Helpers
import Material.Ripple as Ripple
import Html.App
import Html.Attributes as Html exposing (class)
import Html.Events as Html

import Dict exposing (Dict)

-- MODEL


{-| Component model.
-}
type alias Model =
  { ripple : Ripple.Model
  , activeTab : Int
  }


{-| Default component model constructor.
-}
defaultModel : Model
defaultModel =
  { ripple = Ripple.model
  , activeTab = 0
  }


-- ACTION, UPDATE


{-| Component action.
-}
type Msg
  = Select Int
  | Ripple Ripple.Msg


{-| Component update.
-}
update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    Select idx ->
      ({ model | activeTab = idx }, none)
    Ripple action' ->
      let
        (nextRipple, cmd) = Ripple.update action' model.ripple
      in
        ({ model | ripple = nextRipple }, Cmd.map (Ripple) cmd)


-- PROPERTIES


type alias Config =
  { ripple : Bool
  }


defaultConfig : Config
defaultConfig =
  { ripple = False
  }


type alias Property m =
  Options.Property Config m



type Panel m =
  Panel
  { styles : List (Property m)
  , content : List (Html m)
  }


type TabLink m =
  TabLink
  { styles : List (Property m)
  , content : List (Html m)
  }


type Tab m =
  Tab
  { panel : Panel m
  , link : TabLink m
  }




tab : { panel : Panel m, link : TabLink m} -> Tab m
tab =
  Tab

panel : List (Property m) -> List (Html m) -> Panel m
panel styles content =
  Panel { styles = styles, content = content}

link : List (Property m) -> List (Html m) -> TabLink m
link styles content =
  TabLink { styles = styles, content = content}

{- See src/Material/Button.elm for an example of, e.g., an onClick handler.
-}

active : Property m
active = cs "is-active"


ripple : Property m
ripple =
  Options.set (\options -> { options | ripple = True })

-- VIEW

{-| Component view.
-}
view : (Msg -> m) -> Model -> List (Property m) -> List (Tab m) -> Html m
view lift model options tabs =

  let
    summary = Options.collect defaultConfig options
    config = summary.config


    unwrapPanel idx (Panel { styles, content }) =
      Options.styled Html.div
        (cs "mdl-tabs__panel"
        :: cs "is-active" `when` (idx == model.activeTab)
        :: styles)
        content


    unwrapLink idx (TabLink { styles, content }) =
      Options.apply summary Html.a
        [ cs "mdl-tabs__tab"
        , cs "is-active" `when` (idx == model.activeTab)
        ]
        [ Just (Helpers.blurOn "mouseup")
        , Just (Helpers.blurOn "mouseleave")
        , Just (Html.onClick (lift (Select idx)))
        ]
        (if config.ripple then
           List.concat
           [ content
           , [ Html.App.map (Ripple >> lift) <| Ripple.view
                 [ class "mdl-tabs__ripple-container"
                 , class "mdl-js-ripple-effect"
                 , Helpers.blurOn "mouseup"
                 ]
                 model.ripple
             ]
           ]
         else
           content)


    unwrapTab idx (Tab { panel, link }) =
      (unwrapPanel idx panel, unwrapLink idx link)

    tabs' = List.indexedMap unwrapTab tabs

    (panels, links') = List.unzip tabs'

    links =
      Options.styled Html.div
        (cs "mdl-tabs__tab-bar" :: [])
        links'

  in
    Options.apply summary Html.div
      [ cs "mdl-tabs"
      , cs "mdl-js-tabs"
      , cs "is-upgraded"
      , cs "mdl-js-ripple-effect" `when` config.ripple
      , cs "mdl-js-ripple-effect--ignore-events" `when` config.ripple
      ]
      [ Just (Helpers.blurOn "mouseup")
      , Just (Helpers.blurOn "mouseleave")
      ]
      (links :: panels)
      --elems
    -- Options.styled Html.div
    --   (cs "mdl-tabs mdl-js-tabs" :: options)
    --   [text "Content"]


-- COMPONENT

type alias Container c =
  { c | tabs : Indexed Model }


{-| Component render.
-}
render
  : (Parts.Msg (Container c) -> m)
  -> Parts.Index
  -> (Container c)
  -> List (Property m)
  -> List (Tab m)
  -> Html m
render =
  Parts.create view update .tabs (\x y -> {y | tabs = x}) defaultModel

{- See src/Material/Layout.mdl for how to add subscriptions. -}
