module Material.Options
    exposing
        ( Property
        , cs
        , css
        , many
        , nop
        , data
        , when
        , maybe
        , disabled
        , styled
        , styled_
        , stylesheet
        , Style
        , div
        , span
        , img
        , attribute
        , center
        , scrim
        , id
        , input
        , container
        , onClick
        , onDoubleClick
        , onMouseDown
        , onMouseUp
        , onMouseEnter
        , onMouseLeave
        , onMouseOver
        , onMouseOut
        , onCheck
        , onToggle
        , onBlur
        , onFocus
        , onInput
        , on
        , on1
        , onWithOptions
        , dispatch
        )

{-| Setting options for Material components.

Here is a standard use of an elm-mdl Textfield:

    import Material.Textfield as Textfield
    import Material.Options as Options

    Textfield.render MDL [0] model.mdl
      [ Textfield.floatingLabel
      , Textfield.label "name"
      , Textfield.value model.value
      , Options.css "width" "96px"
      , Options.cs "my-textfield-class"
      , Options.onInput MyReceiveInputMsg
      ]
      []

The above code renders a textfield, setting the optional properties
`floatingLabel` and `label "name"` on the textfield; as well as adding
additional (CSS) styling `width: 96px;` and the HTML class `my-name-textfield`.

Some optional properties apply to all components and some are
particular to a specific component. In the above example
`Textfield.floatingLabel`, `Textfield.label`, and `Textfield.value` are
specific to `Textfield`, whereas `Options.css`, `Options.cs`, and
`Options.onInput` apply to any component.

This module contains some very common universally applicable optional
properties such as `css`, `cs`, and `id`. In addition, some elm-mdl modules
expose such options, e.g., `Typography`, `Elevation`, `Badge`, and `Color`.
Universally applicable optional properties can _also_ be applied to standard
`Html` elements such as `Html.div`; see `style` et. al. below. This is
convenient, e.g., for applying elm-mdl typography or color to standard
elements.


# Optional property

@docs Property

# Constructors
@docs cs, css, data, many, nop, when, maybe

# Html
@docs Style, styled, styled_

## Elements
@docs div, span, img
@docs stylesheet

## Attributes
@docs attribute, id
@docs disabled

## Helpers
@docs center, scrim

# Event handlers
@docs onClick, onDoubleClick,
      onMouseDown, onMouseUp,
      onMouseEnter, onMouseLeave,
      onMouseOver, onMouseOut
@docs onCheck, onToggle
@docs onBlur, onFocus
@docs onInput

## Custom Event Handlers
@docs on, on1
@docs onWithOptions

# Advanced usage

## Option distribution
Some components (notably textfields & toggles) are implemented as `<input>`
elements sitting inside a container `<div>` alongside various helper elements.
Options to such components are distributed between input and container elements
as follows:

<table>
  <tr>
    <th>Option</th><th>Element</th>
  </tr>
  <tr> <td>`Options.id`         </td><td> input     </td> </tr> 
  <tr> <td>`Options.css`        </td><td> container </td> </tr>
  <tr> <td>`Options.cs`         </td><td> container </td> </tr>
  <tr> <td>`Options.attributes` </td><td> input     </td> </tr>
  <tr> <td>`Options.on*`        </td><td> input     </td> </tr>
</table>

If you need an option to apply to the other element, use either `Options.input
options` to force `options` to be applied to the input element; or
`Options.container options` to force `options` to be applied to the container
element.

@docs input, container

## Multiple dispatch

@docs dispatch
-}

import Html exposing (Html, Attribute)
import Html.Attributes
import Html.Events
import Json.Decode as Json
import Material.Options.Internal as Internal exposing (..)



-- PROPERTIES


{-|
Type of elm-mdl optional properties. (Do not confuse these with Html properties
or `Html.Attributes.property`.)

The type variable `c` identifies the component the property is for. You never
have to set it yourself. The type variable `m` is the type of your messages
carried by the optional property, if applicable. You should set this yourself.

The elements of the penultimate argument in the above call to
`Textfield.render` has this type, specifically:

    List (Property (Textfield.Config) Msg)
-}
type alias Property c m =
    Internal.Property c m


{-| Universally applicable elm-mdl properties, e.g., `Options.css`,
`Typography.*`, or `Options.onClick`, may be applied to ordinary `Html` values
such as `Html.h4` using `styled` below.
-}
type alias Style m =
    Property () m


{-| Apply properties to a standard Html element.
-}
styled : (List (Attribute m) -> a) -> List (Property c m) -> a
styled ctor props =
    ctor
        (addAttributes
            (collect_ props)
            []
        )


{-| Apply properties and attributes to a standard Html element.
-}
styled_ : (List (Attribute m) -> a) -> List (Property c m) -> List (Attribute m) -> a
styled_ ctor props attrs =
    ctor
        (addAttributes
            (collect_ props)
            attrs
        )


{-| Convenience function for the ultra-common case of apply elm-mdl styling to a
`div` element. Use like this:

    myDiv : Html m
    myDiv =
      Options.div
        [ Color.background Color.primary
        , Color.text Color.accentContrast
        ]
        [ text "I'm in color!" ]

-}
div : List (Property c m) -> List (Html m) -> Html m
div =
    styled Html.div


{-| Convenience function for the reasonably common case of setting attributes
of a span element. See also `div`.
-}
span : List (Property c m) -> List (Html m) -> Html m
span =
    styled Html.span


{-| Convenience function for the not unreasonably uncommon case of setting
attributes of an img element. Use like this:

    img
      [ Options.css "height" "200px" ]
      [ Html.Attributes.src "assets/image.jpg" ]
-}
img : List (Property a b) -> List (Attribute b) -> Html b
img options attrs =
    styled_ Html.img options attrs []


{-| Set HTML disabled attribute.
-}
disabled : Bool -> Property c m
disabled v =
    Attribute (Html.Attributes.disabled v)


{-| Add an HTML class to a component. (Name chosen to avoid clashing with
Html.Attributes.class.)
-}
cs : String -> Property c m
cs c =
    Class c


{-| Add a CSS style to a component.
-}
css : String -> String -> Property c m
css key value =
    CSS ( key, value )


{-| Multiple options.
-}
many : List (Property c m) -> Property c m
many =
    Many


{-| Do nothing. Convenient when the absence or
presence of Options depends dynamically on other values, e.g.,

    Options.div
      [ if model.isActive then cs "active" else nop ]
      [ ... ]
-}
nop : Property c m
nop =
    None


{-| HTML data-* attributes. Prefix "data-" is added automatically.
-}
data : String -> String -> Property c m
data key val =
    Attribute (Html.Attributes.attribute ("data-" ++ key) val)


{-| Conditional option. When the guard evaluates to `true`, the option is
applied; otherwise it is ignored. Use like this:

    Button.disabled |> when (not model.isRunning)
-}
when : Bool -> Property c m -> Property c m
when guard prop  =
    if guard then
        prop
    else
        nop


{-| Apply a Maybe option when defined
-}
maybe : Maybe (Property c m) -> Property c m
maybe prop =
    prop |> Maybe.withDefault nop



-- CONVENIENCE


{-| Construct an Html element contributing to the global stylesheet.
The resulting Html is a `<style>` element.  Remember to insert the resulting Html
somewhere.
-}
stylesheet : String -> Html m
stylesheet css =
    Html.node "style" [] [ Html.text css ]


-- STYLE


{-| Install arbitrary `Html.Attribute`.

    Options.div
      [ Options.attribute <| Html.Attributes.title "title" ]
      [ ... ]

**NB!** Do not install event handlers using `Options.attribute`.
Instead use `Options.on` and variants.
-}
attribute : Html.Attribute m -> Property c m
attribute =
    Attribute


{-| Install arbitrary `Html.Attribute`. Use like this:

    Options.div
      [ Options.attr <| Html.onClick MyClickEvent ]
      [ ... ]

**NOTE** Some attributes might be overridden by attributes
used internally by *elm-mdl*. Such attributes often include
`focus` and `blur` on certain elements, such as `Textfield`.
In the case of `focus` and `blur` you may use `focusin` and `focusout`
respectively instead (these attributes require polyfill on Firefox).

See [Textfield.onBlur](http://package.elm-lang.org/packages/debois/elm-mdl/latest/Material-Textfield#onBlur) for more information regarding the polyfill.
-}
attr : Html.Attribute m -> Property c m
attr =
    Attribute


{-| Options installing css for element to be a flex-box container centering its
elements.
-}
center : Property c m
center =
    many
        [ css "display" "flex"
        , css "align-items" "center"
        , css "justify-content" "center"
        ]


{-| Scrim. Argument value indicates terminal opacity, the value of which should
depend on the underlying image. `0.6` works well often.
-}
scrim : Float -> Property c m
scrim opacity =
    css "background" <|
        "linear-gradient(rgba(0, 0, 0, 0), rgba(0, 0, 0, "
            ++ toString opacity
            ++ "))"


{-| Sets the id attribute
-}
id : String -> Property c m
id =
    Attribute << Html.Attributes.id


{-| Apply argument options to `input` element in component implementation.
-}
input : List (Style m) -> Property (Input c m) m
input =
    Internal.input


{-| Apply argument options to container element in component implementation.
-}
container : List (Style m) -> Property (Container c m) m
container =
    Internal.container



-- EVENTS


{-| Add custom event handlers
-}
on : String -> Json.Decoder m -> Property c m
on event =
    Listener event Nothing


{-| Add a custom event handler that always succeeds.

Equivalent to `Options.on event (Json.Decode.succeed msg)`
-}
on1 : String -> m -> Property c m
on1 event m =
    on event (Json.succeed m)


{-| -}
onClick : msg -> Property c msg
onClick msg =
    on "click" (Json.succeed msg)


{-| -}
onDoubleClick : msg -> Property c msg
onDoubleClick msg =
    on "dblclick" (Json.succeed msg)


{-| -}
onMouseDown : msg -> Property c msg
onMouseDown msg =
    on "mousedown" (Json.succeed msg)


{-| -}
onMouseUp : msg -> Property c msg
onMouseUp msg =
    on "mouseup" (Json.succeed msg)


{-| -}
onMouseEnter : msg -> Property c msg
onMouseEnter msg =
    on "mouseenter" (Json.succeed msg)


{-| -}
onMouseLeave : msg -> Property c msg
onMouseLeave msg =
    on "mouseleave" (Json.succeed msg)


{-| -}
onMouseOver : msg -> Property c msg
onMouseOver msg =
    on "mouseover" (Json.succeed msg)


{-| -}
onMouseOut : msg -> Property c msg
onMouseOut msg =
    on "mouseout" (Json.succeed msg)


{-| Capture [change](https://developer.mozilla.org/en-US/docs/Web/Events/change)
events on checkboxes. It will grab the boolean value from `event.target.checked`
on any input event.
Check out [targetChecked](#targetChecked) for more details on how this works.
-}
onCheck : (Bool -> msg) -> Property c msg
onCheck =
    (flip Json.map Html.Events.targetChecked) >> on "change"


{-| -}
onToggle : msg -> Property c msg
onToggle =
    on1 "change"



-- FOCUS EVENTS


{-| -}
onBlur : msg -> Property c msg
onBlur msg =
    on "blur" (Json.succeed msg)


{-| -}
onFocus : msg -> Property c msg
onFocus msg =
    on "focus" (Json.succeed msg)


{-| -}
onInput : (String -> m) -> Property c m
onInput f =
    on "input" (Json.map f Html.Events.targetValue)


{-| Add custom event handlers with options
-}
onWithOptions : String -> Html.Events.Options -> Json.Decoder m -> Property c m
onWithOptions evt options =
    Listener evt (Just options)



-- DISPATCH


{-| No-shorthand multiple-event dispatch.

NB! You are _extremely_ unlikely to need this.

You need this optional property in exactly these circumstances:
1. You are using an elm-mdl component which has a `render` function.
2. You are not using this `render` function, instead calling `view`.
3. You installed an `on*` handler on the component, but that handler does not
seem to take effect.

What's happening in this case is that elm-mdl has an internal handler for the
same event as your custom handler; e.g., you install `onBlur` on
`Textfield`, but `Textfield`'s has an internal `onBlur` handler.

In this case you need to tell the component how to dispatch multiple messages
(one for you, one for itself) in response to a single DOM event. You do so by
providing a means of folding a list of messages into a single message. (See
the [Dispatch](https://github.com/vipentti/elm-dispatch) library for one way to define such a function.)

The `render` function does all this automatically. If you are calling `render`,
you do not need this property.

Example use:


    type Msg =
      ...
      | Textfield (Textfield.Msg)
      | MyBlurMsg
      | Batch (List Msg)

    ...

      Textfield.view Textfield model.textfield
        [ Options.dispatch Batch
        , Options.onBlur MyBlurMsg
        ]
        [ ]
-}
dispatch : (List m -> m) -> Property c m
dispatch =
    Json.map >> Lift
