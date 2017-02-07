module Material.Dropdown
    exposing
        ( Property
        , Alignment(..)
        , bottomLeft
        , bottomRight
        , topLeft
        , topRight
        , over
        , below
        , index

        , Item
        , item

        , render
        , react

        , Config
        , defaultConfig

        , defaultIndex

        , Model
        , defaultModel
        , Msg(..)
        , update
        , view
        )

{-| This component implements a generic dropdown component. It is used by
Material.Menu and Material.Select.

# Render
@docs render

# Options
@docs Property

# Alignment
@docs Alignment, bottomLeft, bottomRight, topLeft, topRight

# Configuration
@docs index

# Item
@docs Item, item

# Config
@docs Config, defaultConfig

# Helpers
@docs defaultIndex

# Elm architecture
@docs Model, defaultModel, Msg, update, view

# Internal use
@docs react
-}

import Dict exposing (Dict)
import Html exposing (Html, Attribute)
import Html.Keyed
import Material.Component as Component exposing (Indexed, Index)
import Material.Dropdown.Geometry as Geometry exposing (Geometry, Element)
import Material.Dropdown.Item as Item
import Material.Helpers as Helpers exposing (pure, map1st)
import Material.Options as Options exposing (cs, css, styled, styled_, when)
import Material.Options.Internal as Internal
import Material.Ripple as Ripple
import Mouse
import String


-- CONSTANTS


constant :
    { transitionDurationSeconds : Float
    , transitionDurationFraction : Float
    , closeTimeout : Float
    }
constant =
    { transitionDurationSeconds = 0.3
    , transitionDurationFraction = 0.8
    , closeTimeout = 150
    }


transitionDuration : Float
transitionDuration =
    constant.transitionDurationSeconds
        * constant.transitionDurationFraction


-- MODEL


{-| Component model
-}
type alias Model =
    { ripples : Dict Int Ripple.Model
    , open : Bool
    , geometry : Maybe Geometry
    , index : Maybe Int
    }


{-| Convenience export from Dropdown.Item
-}
type alias Item m =
  Item.Model m


{-| Convenience export from Dropdown.Item
-}
item : List (Item.Property m) -> List (Html m) -> Item.Model m
item =
  Item.item


{-| Default component model
-}
defaultModel : Model
defaultModel =
    { ripples = Dict.empty
    , open = False
    , geometry = Nothing
    , index = Nothing
    }


-- ACTION, UPDATE


{-| Component action.
-}
type Msg m
    = Open Geometry
    | ItemMsg ItemIndex (Item.Msg m)
    | Close
    | Click Alignment Mouse.Position
    | Key (Maybe ItemIndex) (List (ItemSummary m)) KeyCode Geometry


{-| The index of an item in the Dropdown's list.
-}
type alias ItemIndex =
  Int


{-| ItemSummary in particular captures an Item's onSelect handler which need to
be passed from view to model to Dispatch it.

TODO: We only need onSelect, to we should refactor this type to have a nicer
type signature for Key.
-}
type alias ItemSummary m =
    Internal.Summary (Item.Config m) m


{-| Int-representation of a key being pressed.
-}
type alias KeyCode =
    Int


{-| Component update.
-}
update : (Msg msg -> msg) -> Msg msg -> Model -> ( Model, Cmd msg )
update fwd msg model =
    case msg of

        Open geometry ->
            { model
                | open = True
                , geometry = Just geometry
                , index = Nothing
            }
                ! []

        Close ->
            { model
                | open = False
                , geometry = Nothing
                , index = Nothing
            }
                ! []

        ItemMsg idx (Item.Select msg) ->
            -- Close the menu after some delay for the ripple effect to show.
            { model
                | index = Just idx
            }
                ! (List.filterMap identity
                    [ Helpers.delay 150 (fwd Close) |> Just
                    , msg |> Maybe.map Helpers.cmd
                    ]
                  )

        ItemMsg idx (Item.Ripple msg) ->
            let
                ( model_, cmd ) =
                    Dict.get idx model.ripples
                        |> Maybe.withDefault Ripple.model
                        |> Ripple.update msg
            in
                { model | ripples = Dict.insert idx model_ model.ripples }
                    ! [ Cmd.map (Item.Ripple >> ItemMsg idx >> fwd) cmd ]

        Click alignment pos ->
            let
                inside { x, y } { top, left, width, height } =
                    (left <= toFloat x)
                        && (toFloat x <= left + width)
                        && (top <= toFloat y)
                        && (toFloat y <= top + height)

                g =
                    Maybe.withDefault Geometry.defaultGeometry model.geometry

                container =
                    g.container.bounds

                input =
                    g.button.bounds
            in
                if model.open && not (inside pos container) then
                    update fwd Close model
                  else
                    model ! []

        Key defaultIndex summaries keyCode g ->
            case keyCode of

                9 ->
                    -- TAB
                    update fwd Close model

                13 ->
                    -- ENTER

                    if model.open then
                            -- TODO: trigger ripple
                            case defaultIndex of
                                Just index ->
                                    let
                                        cmd =
                                            List.drop index summaries
                                                |> List.head
                                                |> Maybe.andThen (.config >> .onSelect)
                                    in
                                        update fwd (ItemMsg index (Item.Select cmd)) model

                                _ ->
                                    update fwd Close model
                        else
                            update fwd (Open g) model

                27 ->
                    -- ESC
                    update fwd Close model

                32 ->
                  -- SPACE, same as ENTER
                  update fwd (Key defaultIndex summaries 13 g) model

                40 ->
                    -- DOWN_ARROW
                    let
                        index =
                            Maybe.withDefault -1 defaultIndex

                        items =
                            List.indexedMap (,) summaries

                        numItems =
                            List.length summaries
                    in
                        (items ++ items)
                            |> List.drop (1 + index)
                            |> List.filter (Tuple.second >> .config >> .enabled)
                            |> List.head
                            |> Maybe.map
                                (Tuple.first
                                    >> \index_ ->
                                        { model
                                            | index = Just index_
                                        }
                                )
                            |> Maybe.withDefault model
                            |> flip (!) []

                38 ->
                    -- UP_ARROW
                    let
                        index =
                            Maybe.withDefault 0 defaultIndex

                        items =
                            List.indexedMap (,) summaries

                        numItems =
                            List.length summaries
                    in
                        (items ++ items)
                            |> List.reverse
                            |> List.drop (numItems - index)
                            |> List.filter (Tuple.second >> .config >> .enabled)
                            |> List.head
                            |> Maybe.map
                                (Tuple.first
                                    >> \index_ ->
                                        { model
                                            | index = Just index_
                                        }
                                )
                            |> Maybe.withDefault model
                            |> pure

                _ ->
                    model ! []


-- PROPERTIES


{-| Dropdown configuration.
-}
type alias Config m =
    { alignment : Alignment
    , index : Maybe Int
    , listeners : List (Maybe Int -> Attribute m)
    }


{-| Menu alignment.
Specifies where the menu opens in relation to the
button, rather than where the menu is positioned.
-}
type Alignment
    = BottomLeft
    | BottomRight
    | TopLeft
    | TopRight
    | Over
    | Below


{-| Default configuration.
Dropdown is aligned BottomLeft.
-}
defaultConfig : Config m
defaultConfig =
    { alignment = BottomLeft
    , index = Nothing
    , listeners = []
    }


{-| Type of Menu options
-}
type alias Property m =
    Options.Property (Config m) m


{-| Menu extends from the bottom-left of the icon.
(Suitable for the menu-icon sitting in a top-left corner)
-}
bottomLeft : Property m
bottomLeft =
    Internal.option (\config -> { config | alignment = BottomLeft })


{-| Menu extends from the bottom-right of the icon.
(Suitable for the menu-icon sitting in a top-right corner)
-}
bottomRight : Property m
bottomRight =
    Internal.option (\config -> { config | alignment = BottomRight })


{-| Menu extends from the top-left of the icon.
(Suitable for the menu-icon sitting in a lower-left corner)
-}
topLeft : Property m
topLeft =
    Internal.option (\config -> { config | alignment = TopLeft })


{-| Menu extends from the rop-right of the icon.
(Suitable for the menu-icon sitting in a lower-right corner)
-}
topRight : Property m
topRight =
    Internal.option (\config -> { config | alignment = TopRight })


{-| Menu extends from the rop-right of the icon and opens inwards.
(Suitable for Select.)
-}
over : Property m
over =
    Internal.option (\config -> { config | alignment = Over })


{-| Menu opens below and extends downwards.
(Suitable for Select.)
-}
below : Property m
below =
    Internal.option (\config -> { config | alignment = Below })


{-| Menu extends from the rop-right of the icon.
(Suitable for the menu-icon sitting in a lower-right corner)
-}
index : Int -> Property m
index v =
    Internal.option (\config -> { config | index = Just v })



-- VIEW


containerGeometry : Alignment
  -> Geometry
  -> { top : Maybe Float
     , left : Maybe Float
     , bottom : Maybe Float
     , right : Maybe Float
     }
containerGeometry alignment geometry =
    case alignment of
        BottomLeft ->
            { top =
                  geometry.button.offsetTop + geometry.button.offsetHeight |> Just
            , left =
                  geometry.menu.offsetLeft |> Just
            , bottom =
                  Nothing
            , right =
                  Nothing
            }
        BottomRight ->
            { top =
                  geometry.button.offsetTop + geometry.button.offsetHeight |> Just
            , left =
                  Nothing
            , bottom =
                  Nothing
            , right =
                let
                    right e =
                        e.bounds.left + e.bounds.width
                in
                    right geometry.container - right geometry.menu |> Just
            }
        TopLeft ->
            { top =
                  Nothing
            , left =
                  geometry.menu.offsetLeft |> Just
            , bottom =
                  let
                      bottom =
                          geometry.container.bounds.top + geometry.container.bounds.height
                  in
                      bottom - geometry.button.bounds.top |> Just
            , right =
                  Nothing
            }
        TopRight ->
            { top =
                  Nothing
            , left =
                  Nothing
            , bottom =
                  let
                      bottom =
                          geometry.container.bounds.top + geometry.container.bounds.height
                  in
                      bottom - geometry.button.bounds.top |> Just
            , right =
                let
                    right e =
                        e.bounds.left + e.bounds.width
                in
                    right geometry.container - right geometry.menu |> Just
            }
        Over ->
            { top =
                  Just 0
            , left =
                  Just (geometry.button.bounds.width - geometry.menu.bounds.width)
            , bottom =
                  Nothing
            , right =
                  Nothing
            }
        Below ->
            { top =
                  Just (geometry.button.bounds.height + 20)
            , left =
                  Just 0
            , bottom =
                  Nothing
            , right =
                  Just 0
            }


applyContainerGeometry : Alignment -> Geometry -> Options.Style m
applyContainerGeometry alignment g =
    let
        f v =
            Maybe.map toPx v
            |> Maybe.withDefault "auto"

        r =
            containerGeometry alignment g
    in
    [ css "top" (f r.top)
    , css "bottom" (f r.bottom)
    , css "left" (f r.left)
    , css "right" (f r.right)
    , css "width" (if alignment == Below then "100%" else (g.menu.bounds.width |> toPx))
    , css "height" (g.menu.bounds.height |> toPx)
    ]
    |> Options.many


clip : Model -> Alignment -> Geometry -> Property m
clip model alignment g =
    let
        width =
            g.menu.bounds.width

        height =
            g.menu.bounds.height
    in
        css "clip" <|
        if model.open then
            if alignment == Below then
                rect 0 g.button.bounds.width height 0
            else
                rect 0 width height 0
          else
            case alignment of

                BottomRight ->
                    rect 0 width 0 width

                TopLeft ->
                    rect height 0 height 0

                TopRight ->
                    rect height width height width

                _ ->
                    ""


{-| The default index captures the notion of the currently selected item, or
the default value of the component. Because the value of the component is
external to Dropdown, it can be specified by config. Because keyboard input
changes this notion (and we do not want to impose keyboard input on the user),
it is also kept in Model. Generally, Model takes precedence over Config and
event listeners allow you to read that value to update your Model accordingly
when the Dropdown closes.
-}
defaultIndex : Model -> Maybe Int -> Maybe Int
defaultIndex model defaultValue =
    if model.index /= Nothing then
          model.index
        else
          defaultValue


{-| Component view.
-}
view
    : (Msg m -> m)
    -> Model
    -> List (Property m)
    -> List (Item m)
    -> Html m
view lift model properties items =
    let
        alignment =
            case config.alignment of
                BottomLeft ->
                    cs "mdl-menu--bottom-left"

                BottomRight ->
                    cs "mdl-menu--bottom-right"

                TopLeft ->
                    cs "mdl-menu--top-left"

                TopRight ->
                    cs "mdl-menu--top-right"

                Over ->
                    cs "mdl-menu--over"

                Below ->
                    cs "mdl-menu--below"

        ({ config } as summary) =
            Internal.collect defaultConfig properties

        g =
            model.geometry
                |> Maybe.withDefault Geometry.defaultGeometry

        container =
            containerGeometry config.alignment g

        menu =
          g.menu.bounds

        fwdRipple =
            Item.Ripple >> ItemMsg -1 >> lift


        numItems =
            List.length items

        defaultIndex_ =
            defaultIndex model config.index

        itemSummaries =
            List.map (Internal.collect Item.defaultConfig << .options) items

    in
      styled Html.div
        [ cs "mdl-menu__container"
        , cs "is-upgraded"
        , when model.open (cs "is-visible")
        , when model.open (applyContainerGeometry config.alignment g)
        ]
        [ styled Html.div
            [ cs "mdl-menu__outline"
            , alignment
            , when (model.open && (config.alignment /= Below))
                (css "width" <| toPx menu.width)
            , when model.open (css "height" <| toPx menu.height)
            ]
            []
        , styled Html.Keyed.ul
            [ cs "mdl-menu"
            , cs "mdl-js-menu"
            , clip model config.alignment g
            , alignment
            ]
            ( let
                view1 index item options =
                    Item.view
                        (ItemMsg index >> lift)
                        model
                        index
                        item
                        (when (Just index == defaultIndex_) Item.selected :: options)

                transitionDelays =
                    List.map2
                        ( \offsetTop offsetHeight ->
                          transitionDelay
                              config.alignment
                              g.menu.bounds.height
                              offsetTop
                              offsetHeight
                        )
                        g.offsetTops
                        g.offsetHeights
              in
                if model.open then
                        List.indexedMap
                            ( \index (item, transitionDelay) ->
                                  view1 index item [ transitionDelay ]
                            )
                            (List.map2 (,) items transitionDelays)
                    else
                        List.indexedMap
                            ( \index item ->
                                  view1 index item []
                            )
                            items
            )
        ]

-- TODO: different from Shared.delay

transitionDelay
    : Alignment
    -> Float
    -> Float
    -> Float
    -> Item.Property m
transitionDelay alignment height offsetTop offsetHeight =
    let
        t =
            if alignment == TopLeft || alignment == TopRight then
                (height - offsetTop - offsetHeight) / height * transitionDuration
            else
                (offsetTop / height * transitionDuration)
    in
        css "transition-delay" <| toString t ++ "s"


-- COMPONENT


type alias Store s =
    { s | menu : Indexed Model }


( get, set ) =
    Component.indexed .menu (\x y -> { y | menu = x }) defaultModel


{-| Component react function. Internal use only.
-}
react :
    (Msg m -> m)
    -> Msg m
    -> Index
    -> Store s
    -> ( Maybe (Store s), Cmd m )
react lift msg idx store =
    update lift msg (get idx store)
        |> map1st (set idx store >> Just)


{-| Component render. Below is an example, assuming boilerplate setup as
indicated in `Material`, and a user message `Select String`.

    Menu.render Mdl [idx] model.mdl
      [ Menu.topLeft, Menu.ripple ]
      [ Menu.item
        [ onSelect Select "Some item" ]
        [ text "Some item" ]
      , Menu.item
        [ onSelect "Another item", Menu.divider ]
        [ text "Another item" ]
      , Menu.item
        [ onSelect "Disabled item", Menu.disabled ]
        [ text "Disabled item" ]
      , Menu.item
        [ onSelect "Yet another item" ]
        [ text "Yet another item" ]
      ]
-}
render :
    (Component.Msg button textfield (Msg m) layout toggles tooltip tabs select dispatch
     -> m
    )
    -> Component.Index
    -> Store s
    -> List (Property m)
    -> List (Item m)
    -> Html m
render =
    Component.render get view Component.MenuMsg


-- HELPERS


rect : Float -> Float -> Float -> Float -> String
rect x y w h =
    [ x, y, w, h ]
        |> List.map toPx
        |> String.join " "
        |> (\coords -> "rect(" ++ coords ++ ")")


toPx : Float -> String
toPx =
    toString >> flip (++) "px"
