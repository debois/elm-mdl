module Material.Menu
    exposing
        ( Model
        , defaultModel
        , Msg
        , update
        , view
        , render
        , react
        , Property
        , bottomLeft
        , bottomRight
        , topLeft
        , topRight
        , ripple
        , icon
        , subscriptions
        , subs
        , Item
        , item
        , divider
        , disabled
        , onSelect
        )

{-| From the [Material Design Lite documentation](http://www.getmdl.io/components/#menus-section):

> The Material Design Lite (MDL) menu component is a user interface element
> that allows users to select one of a number of options. The selection
> typically results in an action initiation, a setting change, or other
> observable effect. Menu options are always presented in sets of two or
> more, and options may be programmatically enabled or disabled as required.
> The menu appears when the user is asked to choose among a series of
> options, and is usually dismissed after the choice is made.

> Menus are an established but non-standardized feature in user interfaces,
> and allow users to make choices that direct the activity, progress, or
> characteristics of software. Their design and use is an important factor in
> the overall user experience. See the menu component's Material Design
> specifications page for details.

See also the
[Material Design Specification]([https://www.google.com/design/spec/components/menus.html).

Refer to
[this site](https://debois.github.io/elm-mdl/#menus)
for a live demo.

# Subscriptions

The Menu component requires subscriptions to arbitrary mouse clicks to be set
up. Example initialisation of containing app:

    import Material.Menu as Menu
    import Material

    type Model =
      { ...
      , mdl : Material.Model -- Boilerplate
      }

    type Msg =
      ...
      | Mdl Material.Msg -- Boilerplate

    ...

    App.program
      { init = init
      , view = view
      , subscriptions = Menu.subs Mdl model
      , update = update
      }

# Render
@docs render, subs

# Items
@docs Item, item, onSelect, disabled, divider

# Options
@docs Property

## Alignment
@docs bottomLeft, bottomRight, topLeft, topRight

## Appearance
@docs ripple, icon

# Elm architecture
@docs Model, defaultModel, Msg, update, view, subscriptions

# Internal use
@docs react

-}

import Dict exposing (Dict)
import Html.Attributes
import Html.Events exposing (defaultOptions)
import Html exposing (..)
import Json.Decode as Json exposing (Decoder)
import Json.Encode exposing (string)
import Mouse
import String
import Material.Helpers as Helpers exposing (pure, map1st)
import Material.Icon as Icon
import Material.Menu.Geometry as Geometry exposing (Geometry)
import Material.Options as Options exposing (Style, cs, css, styled, styled_, when)
import Material.Options.Internal as Internal
import Material.Ripple as Ripple
import Material.Component as Component exposing (Indexed, Index)


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


{-| Component subscriptions.
-}
subscriptions : Model -> Sub (Msg m)
subscriptions model =
    if model.animationState == Opened then
        Mouse.clicks Click
    else
        Sub.none



-- MODEL


{-| Component model
-}
type alias Model =
    { ripples : Dict Int Ripple.Model
    , animationState : AnimationState
    , geometry : Maybe Geometry
    , index : Maybe Int
    }


type AnimationState
    = Idle
    | Opening
    | Opened
    | Closing


{-| Default component model
-}
defaultModel : Model
defaultModel =
    { ripples = Dict.empty
    , animationState = Idle
    , geometry = Nothing
    , index = Nothing
    }



-- ITEM


{-| Type of menu items
-}
type alias Item m =
    { options : List (Options.Property (ItemConfig m) m)
    , html : List (Html m)
    }


{-| Construct a menu item.
-}
item : List (Options.Property (ItemConfig m) m) -> List (Html m) -> Item m
item =
    Item


type alias ItemConfig m =
    { enabled : Bool
    , divider : Bool
    , onSelect : Maybe m
    }


defaultItemConfig : ItemConfig m
defaultItemConfig =
    { enabled = True
    , divider = False
    , onSelect = Nothing
    }


{-| Render a dividing line before the item
-}
divider : Options.Property (ItemConfig m) m
divider =
    Internal.option (\config -> { config | divider = True })


{-| Mark item as disabled.
-}
disabled : Options.Property (ItemConfig m) m
disabled =
    Internal.option (\config -> { config | enabled = False })


{-| Handle selection of containing item
-}
onSelect : m -> Options.Property (ItemConfig m) m
onSelect =
    Internal.option << (\msg config -> { config | onSelect = Just msg })


-- ACTION, UPDATE


{-| Component action.
-}
type Msg m
    = Open Geometry
    | Select Int (Maybe m)
    | Close
    | Tick
    | Ripple Int Ripple.Msg
    | Click Mouse.Position
    | Key (List (Internal.Summary (ItemConfig m) m)) Int


isActive : Model -> Bool
isActive model =
    (model.animationState == Opened) || (model.animationState == Opening)


{-| Component update.
-}
update : (Msg msg -> msg) -> Msg msg -> Model -> ( Model, Cmd msg )
update fwd msg model =
    case msg of
        Open geometry ->
            ( { model
                | animationState =
                    case model.animationState of
                        Opened ->
                            Opened

                        _ ->
                            Opening
                , geometry = Just geometry
              }
            , Helpers.cmd (fwd Tick)
            )

        Tick ->
            { model | animationState = Opened } |> pure

        Close ->
            { model
                | animationState = Idle
                , geometry = Nothing
                , index = Nothing
            }
                |> pure

        Select idx msg ->
            -- Close the menu after some delay for the ripple effect to show.
            let
                model_ =
                    { model | animationState = Closing }

                cmds =
                    List.filterMap identity
                        [ Helpers.delay constant.closeTimeout (fwd Close) |> Just
                        , msg |> Maybe.map Helpers.cmd
                        ]
            in
                ( model_, Cmd.batch cmds )
                

        Ripple idx action ->
            let
                ( model_, effects ) =
                    Dict.get idx model.ripples
                        |> Maybe.withDefault Ripple.model
                        |> Ripple.update action
            in
                ( { model | ripples = Dict.insert idx model_ model.ripples }
                , Cmd.map (Ripple idx >> fwd) effects
                )

        Click pos ->
            if isActive model then
                case model.geometry of
                    Just geometry ->
                        let
                            inside { x, y } { top, left, width, height } =
                                (left <= toFloat x)
                                    && (toFloat x <= left + width)
                                    && (top <= toFloat y)
                                    && (toFloat y <= top + height)
                        in
                            if inside pos geometry.menu.bounds then
                                model ! []
                            else
                                update fwd Close model

                    Nothing ->
                        model ! []
            else
                model ! []

        Key summaries keyCode ->
            case keyCode of
                13 ->
                    -- ENTER
                    if isActive model then
                        case model.index of
                            Just index ->
                                let
                                    cmd =
                                        List.drop index summaries
                                            |> List.head
                                            |> Maybe.andThen (.config >> .onSelect)
                                in
                                    update fwd (Select (index + 1) cmd) model

                            Nothing ->
                                update fwd Close model
                    else
                        model ! []

                27 ->
                    -- ESCAPE
                    update fwd Close model

                32 ->
                    -- SPACE
                    if isActive model then
                        update fwd (Key summaries 13) model
                    else
                        model ! []

                40 ->
                    -- DOWN_ARROW
                    if isActive model then
                        let
                            items =
                                List.indexedMap (,) summaries
                        in
                            (items ++ items)
                                |> List.drop (1 + Maybe.withDefault -1 model.index)
                                |> List.filter (Tuple.second >> .config >> .enabled)
                                |> List.head
                                |> Maybe.map (Tuple.first >> \index_ -> { model | index = Just index_ })
                                |> Maybe.withDefault model
                                |> flip (!) []
                    else
                        model ! []

                38 ->
                    -- UP_ARROW
                    if isActive model then
                        let
                            items =
                                List.indexedMap (,) summaries
                        in
                            (items ++ items)
                                |> List.reverse
                                |> List.drop ((List.length summaries) - Maybe.withDefault 0 model.index)
                                |> List.filter (Tuple.second >> .config >> .enabled)
                                |> List.head
                                |> Maybe.map (Tuple.first >> \index_ -> { model | index = Just index_ })
                                |> Maybe.withDefault model
                                |> pure
                    else
                        model ! []

                _ ->
                    model ! []



-- PROPERTIES


{-| Menu alignment.
Specifies where the menu opens in relation to the
button, rather than where the menu is positioned.
-}
type Alignment
    = BottomLeft
    | BottomRight
    | TopLeft
    | TopRight


type alias Config =
    { alignment : Alignment
    , ripple : Bool
    , icon : String
    }


defaultConfig : Config
defaultConfig =
    { alignment = BottomLeft
    , ripple = False
    , icon = "more_vert"
    }


{-| Type of Menu options
-}
type alias Property m =
    Options.Property Config m


{-| Menu items ripple when clicked
-}
ripple : Property m
ripple =
    Internal.option (\config -> { config | ripple = True })


{-| Set the menu icon
-}
icon : String -> Property m
icon =
    Internal.option << (\name config -> { config | icon = name })


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



-- VIEW


withGeometry : Model -> (Geometry -> Property m) -> Property m
withGeometry model f =
    model.geometry
        |> Maybe.map f
        |> Maybe.withDefault Options.nop


containerGeometry : Alignment -> Geometry -> Property m
containerGeometry alignment geometry =
    [ css "width" <| toPx geometry.menu.bounds.width
    , css "height" <| toPx geometry.menu.bounds.height
    , if (alignment == BottomRight) || (alignment == BottomLeft) then
        css "top" <| toPx (geometry.button.offsetTop + geometry.button.offsetHeight)
      else
        Options.nop
    , if (alignment == BottomRight) || (alignment == TopRight) then
        let
            right e =
                e.bounds.left + e.bounds.width
        in
            css "right" <| toPx (right geometry.container - right geometry.menu)
      else
        Options.nop
    , if (alignment == TopLeft) || (alignment == TopRight) then
        let
            bottom =
                geometry.container.bounds.top + geometry.container.bounds.height
        in
            css "bottom" <| toPx (bottom - geometry.button.bounds.top)
      else
        Options.nop
    , if (alignment == TopLeft) || (alignment == BottomLeft) then
        css "left" <| toPx geometry.menu.offsetLeft
      else
        Options.nop
    ]
        |> Options.many


clip : Model -> Config -> Geometry -> Property m
clip model config geometry =
    let
        width =
            geometry.menu.bounds.width

        height =
            geometry.menu.bounds.height
    in
        css "clip" <|
            if
                (model.animationState == Opened)
                    || (model.animationState == Closing)
            then
                rect 0 width height 0
            else
                case config.alignment of
                    BottomRight ->
                        rect 0 width 0 width

                    TopLeft ->
                        rect height 0 height 0

                    TopRight ->
                        rect height width height width

                    _ ->
                        ""


{-| Component view.
-}
view : (Msg m -> m) -> Model -> List (Property m) -> List (Item m) -> Html m
view lift model properties items =
    let
        summary =
            Internal.collect defaultConfig properties

        config =
            summary.config

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

        numItems =
            List.length items

        itemSummaries =
            List.map (Internal.collect defaultItemConfig << .options) items
    in
        Internal.apply summary
            div
            (css "position" "relative" :: properties)
            []
            [ styled button
                [ cs "mdl-button"
                , cs "mdl-js-button"
                , cs "mdl-button--icon"
                , when 
                    (isActive model) 
                    (onKeyDown (Key itemSummaries))
                , when 
                    (model.animationState /= Opened) 
                    (Options.on "click" (Json.map Open Geometry.decode))
                , when (isActive model) (Options.onClick Close)
                ]
                [ Icon.view config.icon
                    [ cs "material-icons"
                    , css "pointer-events" "none"
                    ]
                ]
                |> Html.map lift
            , styled div
                [ cs "mdl-menu__container"
                , cs "is-upgraded"
                , when ((model.animationState == Opened) || (model.animationState == Closing)) (cs "is-visible")
                , containerGeometry config.alignment |> withGeometry model
                ]
                [ styled div
                    [ cs "mdl-menu__outline"
                    , alignment
                    , (\geometry ->
                        [ css "width" <| toPx geometry.menu.bounds.width
                        , css "height" <| toPx geometry.menu.bounds.height
                        ]
                            |> Options.many
                      )
                        |> withGeometry model
                    ]
                    []
                , styled ul
                    [ cs "mdl-menu"
                    , cs "mdl-js-menu"
                    , when
                        ((model.animationState == Opening)
                            || (model.animationState == Closing)
                        )
                        (cs "is-animating")
                    , clip model config |> withGeometry model
                    , alignment
                    ]
                    (case model.geometry of
                        Just g ->
                            List.map5
                                (view1 lift config model)
                                g.offsetTops
                                g.offsetHeights
                                (List.range 0 (numItems - 1))
                                itemSummaries
                                items

                        Nothing ->
                            List.map3
                                (view1 lift config model 0 0)
                                (List.range 0 (numItems - 1))
                                itemSummaries
                                items
                    )
                ]
            ]


delay : Alignment -> Float -> Float -> Float -> Options.Property (ItemConfig m) m
delay alignment height offsetTop offsetHeight =
    let
        t =
            if alignment == TopLeft || alignment == TopRight then
                (height - offsetTop - offsetHeight) / height * transitionDuration
            else
                (offsetTop / height * transitionDuration)
    in
        css "transition-delay" <| toString t ++ "s"


view1 :
    (Msg m -> m)
    -> Config
    -> Model
    -> Float
    -> Float
    -> Int
    -> Internal.Summary (ItemConfig m) m
    -> Item m
    -> Html m
view1 lift config model offsetTop offsetHeight index summary item =
    let
        ripple =
            Ripple index >> lift

        canSelect =
            summary.config.enabled
                && summary.config.onSelect
                /= Nothing

        hasRipple =
            config.ripple && canSelect
    in
        Internal.apply summary
            li
            [ cs "mdl-menu__item"
            , cs "mdl-js-ripple-effect" |> when config.ripple
            , cs "mdl-menu__item--full-bleed-divider" |> when summary.config.divider
            , css "background-color" "rgb(238,238,238)" |> when (model.index == Just index)
            , case ( model.geometry, isActive model ) of
                ( Just g, True ) ->
                    delay config.alignment g.menu.bounds.height offsetTop offsetHeight

                _ ->
                    Options.nop
              -- Not in MDL, but convenient to align icons etc.
            , css "display" "flex"
            , css "align-items" "center"
            , Options.onClick (Select index summary.config.onSelect |> lift)
                |> when canSelect
            , when (not summary.config.enabled) 
                <| (Internal.attribute
                      (Html.Attributes.attribute "disabled" "disabled"))
            , Internal.attribute <| Html.Attributes.property "tabindex" (string "-1")
            , if hasRipple then
                Options.many
                    [ Internal.attribute <| Ripple.downOn_ ripple "mousedown"
                    , Internal.attribute <| Ripple.downOn_ ripple "touchstart"
                    , Internal.attribute <| Ripple.upOn_ ripple "mouseup"
                    , Internal.attribute <| Ripple.upOn_ ripple "mouseleave"
                    , Internal.attribute <| Ripple.upOn_ ripple "touchend"
                    , Internal.attribute <| Ripple.upOn_ ripple "blur"
                    ]
              else
                Options.nop
            ]
            []
            (if hasRipple then
                ((++)
                    item.html
                    [ Ripple.view_
                        [ Html.Attributes.class "mdl-menu__item-ripple-container" ]
                        (Dict.get index model.ripples
                            |> Maybe.withDefault Ripple.model
                        )
                        |> Html.map ripple
                    ]
                )
             else
                item.html
            )





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


{-| TODO
-}
subs :
    (Component.Msg button textfield (Msg msg) layout toggles tooltip tabs select dispatch
     -> msg
    )
    -> Store s
    -> Sub msg
subs =
    Component.subs Component.MenuMsg .menu subscriptions



-- HELPERS


onClick : Decoder Geometry -> (Geometry -> m) -> Attribute m
onClick decoder action =
    Html.Events.on "click" (Json.map action decoder)


onKeyDown : (Int -> m) -> Options.Property c m
onKeyDown action =
    Options.onWithOptions
        "keydown"
        { preventDefault = True
        , stopPropagation = False
        }
        (Json.map action Html.Events.keyCode)


rect : Float -> Float -> Float -> Float -> String
rect x y w h =
    [ x, y, w, h ]
        |> List.map toPx
        |> String.join " "
        |> (\coords -> "rect(" ++ coords ++ ")")


toPx : Float -> String
toPx =
    toString >> flip (++) "px"
