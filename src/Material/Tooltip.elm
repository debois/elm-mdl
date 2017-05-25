module Material.Tooltip
    exposing
        ( Model
        , defaultModel
        , Msg(..)
        , update
        , view
        , Property
        , render
        , react
        , attach
        , left
        , right
        , top
        , bottom
        , large
        , onEnter
        , onLeave
        , element
        )

{-| From the [Material Design Lite documentation](https://getmdl.io/components/index.html#tooltips-section):

> The Material Design Lite (MDL) tooltip component is an enhanced version of the
> standard HTML tooltip as produced by the `title` attribute. A tooltip consists
> of text and/or an image that clearly communicates additional information about
> an element when the user hovers over or, in a touch-based UI, touches the
> element. The MDL tooltip component is pre-styled (colors, fonts, and other
> settings are contained in material.min.css) to provide a vivid, attractive
> visual element that displays related but typically non-essential content,
> e.g., a definition, clarification, or brief instruction.
>
> Tooltips are a ubiquitous feature of most user interfaces, regardless of a
> site's content or function. Their design and use is an important factor in the
> overall user experience. See the tooltip component's Material Design
> specifications page for details.

See also the
[Material Design Specification](https://material.google.com/components/tooltips.html).

Refer to [this site](http://debois.github.io/elm-mdl/#tooltips)
for a live demo.

To use a `tooltip` you have to (a) attach the mouse event listeners to the target
by calling `attach`, and (b) create a tooltip with element `Tooltip.render`
as a sibling of the target. Here is an example:

```elm
import Material.Tooltip as Tooltip
import Material.Icon as Icon

tooltip : Model -> Html Msg
tooltip model =
  div []
    [ Icon.view "add" [ Tooltip.attach Mdl [0] ]
    , Tooltip.render Mdl [0] model.mdl
        [Tooltip.top]
        [text "Default tooltip"]
    ]
```

# Render
@docs attach, render

# Options
@docs Property
@docs left, right, top, bottom
@docs large
@docs element

# Elm architecture
If you do not use parts, you should not use `attach`, but instead add the
`onEnter` and `onLeave` attributes to the target element.

@docs onEnter, onLeave
@docs Model, defaultModel, Msg, update, view

# Internal use
@docs react
-}

import Platform.Cmd exposing (Cmd, none)
import Html exposing (..)
import Material.Component as Component exposing (Indexed, Index)
import Material.Options as Options exposing (Style, cs, css, when)
import Material.Options.Internal as Internal
import DOM
import Html.Events
import Json.Decode as Json exposing (field, at)
import String


-- MODEL


{-| Component model.
-}
type alias Model =
    { isActive : Bool
    , domState : DOMState
    }


{-| Default component model constructor.
-}
defaultModel : Model
defaultModel =
    { isActive = False
    , domState = defaultDOMState
    }



-- ACTION, UPDATE


{-| Component message.
-}
type Msg
    = Enter DOMState
    | Leave


{-| Tooltip position
-}
type alias Pos =
    { left : Float
    , top : Float
    , marginLeft : Float
    , marginTop : Float
    }


{-| Default position constructor
-}
defaultPos : Pos
defaultPos =
    { left = 0
    , top = 0
    , marginLeft = 0
    , marginTop = 0
    }


{-| Position and offsets from dom events for the tooltip
-}
type alias DOMState =
    { rect : DOM.Rectangle
    , offsetWidth : Float
    , offsetHeight : Float
    }


{-| Default DOMState constructor
-}
defaultDOMState : DOMState
defaultDOMState =
    { rect = { left = 0, top = 0, width = 0, height = 0 }
    , offsetWidth = 0
    , offsetHeight = 0
    }


{-| Calculates the position of the tooltip based on the event
and the requested position
-}
calculatePos : Position -> DOMState -> Pos
calculatePos pos domState =
    let
        props =
            domState.rect

        offsetWidth =
            domState.offsetWidth

        offsetHeight =
            domState.offsetHeight

        left =
            props.left + (props.width / 2)

        top =
            props.top + (props.height / 2)

        marginLeft =
            -1 * (offsetWidth / 2)

        marginTop =
            -1 * (offsetHeight / 2)

        -- Returns the values if their sum is above 0
        getValuesFor l r =
            if ((l + r) < 0) then
                ( 0, 0 )
            else
                ( l, r )

        ( newTop, newMarginTop ) =
            getValuesFor top marginTop

        ( newLeft, newMarginLeft ) =
            getValuesFor left marginLeft

        out =
            case pos of
                Left ->
                    { left = props.left - offsetWidth - 10
                    , top = newTop
                    , marginTop = newMarginTop
                    , marginLeft = 0
                    }

                Right ->
                    { left = props.left + props.width + 10
                    , top = newTop
                    , marginTop = newMarginTop
                    , marginLeft = 0
                    }

                Top ->
                    { left = newLeft
                    , top = props.top - offsetHeight - 10
                    , marginTop = 0
                    , marginLeft = newMarginLeft
                    }

                Bottom ->
                    { left = newLeft
                    , top = props.top + props.height + 10
                    , marginTop = 0
                    , marginLeft = newMarginLeft
                    }
    in
        out


{-| Component update.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Enter dom ->
            ( { model | isActive = True, domState = dom }, none )

        Leave ->
            ( { model | isActive = False }, none )


{-| Tries and get the next sibling that is available and use the given decoder on it
-}
sibling : Json.Decoder a -> Json.Decoder a
sibling d =
    let
        createPath depth =
            let
                parents =
                    List.repeat depth "parentElement"
            in
                ([ "target" ] ++ parents ++ [ "nextSibling" ])

        paths =
            List.map createPath <| List.range 0 4

        -- Tries to check if the element is actually a tooltip
        valid path =
            isTooltipClass path
                |> Json.andThen
                    (\res ->
                        if res then
                            at path d
                        else
                            Json.fail ""
                    )
    in
        Json.oneOf (List.map valid paths)


{-| Checks if the target at path is an actual tooltip
-}
isTooltipClass : List String -> Json.Decoder Bool
isTooltipClass path =
    (at path DOM.className)
        |> Json.andThen
            (\class ->
                if String.contains "mdl-tooltip" class then
                    Json.succeed True
                else
                    Json.succeed False
            )


{-| Decodes a DOMState from a DOM event
-}
stateDecoder : Json.Decoder DOMState
stateDecoder =
    Json.map3 DOMState
        (DOM.target DOM.boundingClientRect)
        (sibling DOM.offsetWidth)
        (sibling DOM.offsetHeight)



-- PROPERTIES


{-| Tooltip size
-}
type Size
    = Default
    | Large


{-| Tooltip position relative to the element
-}
type Position
    = Left
    | Right
    | Top
    | Bottom


{-| Helper for a `Html m` function. e.g. `Html.div`
-}
type alias HtmlElement a =
    List (Attribute a) -> List (Html a) -> Html a


{-| Tooltip config
-}
type alias Config a =
    { size : Size
    , position : Position
    , elem : HtmlElement a
    }


{-| Default configuration for tooltip
-}
defaultConfig : Config m
defaultConfig =
    { size = Default
    , position = Bottom
    , elem = Html.div
    }


{-| Properties for Tooltip options.
-}
type alias Property m =
    Options.Property (Config m) m


{-| Position the tooltip on the left of the target element
-}
left : Property m
left =
    Internal.option (\options -> { options | position = Left })


{-| Position the tooltip on the right of the target element
-}
right : Property m
right =
    Internal.option (\options -> { options | position = Right })


{-| Position the tooltip above the target element
-}
top : Property m
top =
    Internal.option (\options -> { options | position = Top })


{-| Position the tooltip below the target element
-}
bottom : Property m
bottom =
    Internal.option (\options -> { options | position = Bottom })


{-| Large tooltip
-}
large : Property m
large =
    Internal.option (\options -> { options | size = Large })


{-| Set the tooltip container element. You are unlikely to need this.

This option simply sets the  container element for the tooltip itself, which
you might want to control for layout purposes. It does not set the element
hovering on which triggers the tooltip; use `attach` to set that.
-}
element : HtmlElement m -> Property m
element elem =
    Internal.option (\options -> { options | elem = elem })


-- VIEW


{-| Component view.
-}
view : (Msg -> m) -> Model -> List (Property m) -> List (Html m) -> Html m
view lift model options content =
    let
        summary =
            Internal.collect defaultConfig options

        config =
            summary.config

        px : Float -> String
        px f =
            (toString f) ++ "px"

        pos =
            if model.isActive then
                calculatePos config.position model.domState
            else
                defaultPos
    in
        Options.styled config.elem
            [ cs "mdl-tooltip"
            , cs "is-active" |> when model.isActive
            , cs "mdl-tooltip--large" |> when (config.size == Large)
            , css "left" (px pos.left) |> when model.isActive
            , css "margin-left" (px pos.marginLeft) |> when model.isActive
            , css "top" (px pos.top) |> when model.isActive
            , css "margin-top" (px pos.marginTop) |> when model.isActive
            ]
            content



-- COMPONENT


type alias Store s =
    { s | tooltip : Indexed Model }


( get, set ) =
    Component.indexed .tooltip (\x y -> { y | tooltip = x }) defaultModel


{-| Component react function
-}
react :
    (Component.Msg button textfield menu layout toggles Msg tabs dispatch -> m)
    -> Msg
    -> Index
    -> Store s
    -> ( Maybe (Store s), Cmd m )
react =
    Component.react get set Component.TooltipMsg (Component.generalise update)


{-| Component render.
-}
render :
    (Component.Msg button textfield menu snackbar toggles Msg tabs dispatch -> m)
    -> Component.Index
    -> Store s
    -> List (Property m)
    -> List (Html m)
    -> Html m
render =
    Component.render get view Component.TooltipMsg


{-| Mouse enter event handler, Component variant
-}
onMouseEnter :
    (Component.Msg button textfield menu snackbar toggles Msg tabs dispatch -> m)
    -> Component.Index
    -> Attribute m
onMouseEnter lift idx =
    Html.Events.on
        "mouseenter"
        (Json.map (Enter >> Component.TooltipMsg idx >> lift) stateDecoder)


{-| Mouse leave event handler, Component variant
-}
onMouseLeave :
    (Component.Msg button textfield menu snackbar toggles Msg tabs dispatch -> m)
    -> Component.Index
    -> Attribute m
onMouseLeave lift idx =
    Html.Events.on
        "mouseleave"
        (Json.succeed (Leave |> Component.TooltipMsg idx |> lift))


{-| Attach event handlers for Component version
-}
attach :
    (Component.Msg button textfield menu snackbar toggles Msg tabs dispatch -> m)
    -> Component.Index
    -> Options.Property c m
attach lift index =
    Options.many
        [ Internal.attribute <| onMouseEnter lift index
        , Internal.attribute <| onMouseLeave lift index
        ]


{-| Mouse enter event handler, TEA variant
-}
onEnter : (Msg -> m) -> Attribute m
onEnter lift =
    Html.Events.on "mouseenter" (Json.map lift <| Json.map Enter <| stateDecoder)


{-| Mouse leave event handler, TEA variant
-}
onLeave : (Msg -> m) -> Attribute m
onLeave lift =
    Html.Events.on "mouseleave" (Json.map lift <| Json.succeed Leave)
