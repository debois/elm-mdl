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
  , activeTab : Int
  }


{-| Default component model constructor.
-}
defaultModel : Model
defaultModel =
  { ripples = Dict.empty
  , activeTab = 0
  }



-- ACTION, UPDATE


{-| Component action.
-}
type Msg
  = Select Int
  | Test
  | MultiClick (List Msg)
  | Ripple Int Ripple.Msg



{-| Component update.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
  case action of
    Test ->
      (Debug.log "TEST" model, none)

    Select idx ->
      ( { model | activeTab = Debug.log "SELECT" idx }, none )

    MultiClick handlers ->
      let
        _  = Debug.log "MULTI" handlers


        updated =
          handlers
            |> List.foldl (\ val acc -> fst <| update val acc) model

      in
        (updated, none)

    Ripple idx action' ->
      let
        ( ripple', cmd ) =
          Dict.get idx model.ripples
            |> Maybe.withDefault Ripple.model
            |> Ripple.update action'
      in
        ( { model | ripples = Dict.insert idx ripple' model.ripples }, Cmd.map (Ripple idx) cmd )



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


type Panel m
  = Panel
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
      { panel : Panel m
      , link : TabLink m
      }


tab : { panel : Panel m, link : TabLink m } -> Tab m
tab =
  Tab


panel : List (Property m) -> List (Html m) -> Panel m
panel styles content =
  Panel { styles = styles, content = content }


link : List (Property m) -> List (Html m) -> TabLink m
link styles content =
  TabLink { styles = styles, content = content }



{- See src/Material/Button.elm for an example of, e.g., an onClick handler. -}


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

--onClick : (a -> m) -> Property m


onClick lift =
  Internal.attribute <| Html.onClick lift


onMaybeClick : m -> m -> Property m
onMaybeClick lift maybe =
  Options.many
    [ Internal.attribute <| Html.on "click" (Json.succeed lift `Json.andThen` (\_ -> Json.succeed maybe))
    --, Internal.attribute <| Html.on "click" (Json.succeed maybe)
    ]


onMultiClick lift handlers =
  Internal.attribute <| Html.onClick ((MultiClick handlers) |> lift)

{-| Component view.
-}
view : (Msg -> m) -> Model -> List (Property m) -> List (Tab m) -> Html m
view lift model options tabs =
  let
    summary =
      Options.collect defaultConfig options

    config =
      summary.config

    unwrapPanel idx (Panel { styles, content }) =
      Options.styled Html.div
        ([ cs "mdl-tabs__panel"
         , cs "is-active" `when` (idx == config.activeTab)
         ] ++ styles)
        content

    -- test idx =
    --   config.onSelectTab
    --     |> Maybe.map (\h -> idx |> h |> Select  |> lift)

    unwrapLink idx (TabLink { styles, content }) =
      Options.styled Html.a
        [ cs "mdl-tabs__tab"
        , cs "is-active" `when` (idx == config.activeTab)
        --, onMultiClick lift [Test, Select idx]
        --, onClick ()
        , config.onSelectTab
          |> Maybe.map (\t -> onClick (t idx))
          |>  Maybe.withDefault Options.nop
        -- , onClick (lift Test)
        -- , onClick (lift <| Select idx)
        --, onMaybeClick (lift <| Select idx) (lift Test)
        -- , Options.many
        --   [ --onClick (lift <| Select idx)
        --       --Options.nop
        --   onClick (lift <| Select idx)
        --   , onClick (lift Test)
        --   -- , config.onSelectTab
        --   --   |> Maybe.map (\ val -> Internal.attribute (val idx))
        --   --   |> Maybe.withDefault Options.nop
        --   ]
        ]
        -- [
        --  --Just (Html.onClick (lift <| Select idx))
        -- -- , config.onSelectTab
        -- --   |> Maybe.map ((|>) idx)
        -- ]
        (if config.ripple then
          List.concat
            [ content
            , [ Ripple.view
                  [ Html.classList
                      [ ( "mdl-tabs__ripple-container", True )
                      , ( "mdl-tabs__ripple-js-effect", True )
                      ]
                  ]
                  (Dict.get idx model.ripples
                    |> Maybe.withDefault Ripple.model
                  )
                  |> Html.App.map (Ripple idx >> lift)
              ]
            ]
         else
          content
        )

    unwrapTab idx (Tab { panel, link }) =
      ( unwrapPanel idx panel, unwrapLink idx link )

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
      -- [ Just <|
      --     ( config.onSelectTab
      --     |> Maybe.map (\s -> Html.onClick (s 0))
      --     |> Maybe.withDefault Helpers.noAttr
      --     )
      -- ]
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
