module Material
    exposing
        ( Model
        , model
        , Msg
        , Container
        , update
        , update_
        , subscriptions
        , init
        )

{-|

Material Design component library for Elm based on Google's
[Material Design Lite](https://www.getmdl.io/).

Click
[here](https://debois.github.io/elm-mdl/)
for a live demo.

This module contains (a) documentation about overall usage and API principles of
elm-mdl and (b) functions for suppressing TEA boilerplate. For a "Getting started"
guide, refer to [the
README](https://github.com/debois/elm-mdl/blob/master/README.md#get-started).


# Using the library.

## Interfacing with CSS

This library depends on the CSS part of Google's Material Design Lite. Your app
will have to load that. See the
[Scheme](http://package.elm-lang.org/packages/debois/elm-mdl/latest/Material-Scheme)
module for exposing details. (The starting point implementations above
load CSS automatically.)

## Color theming

Material Design defines a color palette. The
[Color](http://package.elm-lang.org/packages/debois/elm-mdl/latest/Material-Color)
module contains exposing various `Property` values and helper functions for working with
this color palette.

## View functions

The view function of most components has this signature:

    view : (Msg -> m) -> Model -> List (Property m)  -> List (Html m) -> Html m

It's helpful to compare this signature to the standard one of `core/html`, e.g.,
`Html.div`:

    div  :                        List (Attribute m) -> List (Html m) -> Html m

1. For technical reasons, rather than using `Html.map f (view ...)`, you
provide the lifting function `f` directly to the component as the first
argument.
2. The `Model` argument is standard for TEA view functions.
3. The `List (Property m)` argument can be thought of as an alternative
to `List (Html.Attribute)`. You customise the behaviour of elm-mdl components
by supplying these `Property m`, much the same way you set attributes of
  `Html.div`. See the
  [Options](http://package.elm-lang.org/packages/debois/elm-mdl/latest/Material-Options)
  module for details.
4. The `List (Html m)` argument is standard: it is the contents of the component,
  e.g., the text inside a button.

NB! We recommend using shorthands to avoid TEA boilerplate, see below. If you are using shorthands, you should never call `view` directly, but rather use `render`.

# Shorthands for TEA components

The component model of the library is simply the Elm Architecture (TEA), i.e.,
each component has types `Model` and `Msg`, and values `view` and `update`. A
minimal example using this library as plain TEA can be found
[here](https://github.com/debois/elm-mdl/blob/master/examples/Counter-no-parts.elm).

Using more than a few component in plain TEA is unwieldy because of the large
amount of boilerplate one has to write. The elm-mdl library supports
shorthands for avoiding most of this boilerplate. A minimal example using
shorthands is
  [here](http://github.com/debois/elm-mdl/blob/master/examples/Counter.elm).

It is important to note that the shorthands are not an alternative to TEA; they
simply do much of the tedious TEA boilerplate. Note that elm-mdl no longer
depends on the [Parts library](https://github.com/debois/elm-parts) and does
not put functions in messages. The elm-mdl library conforms to [component
guidelines](https://github.com/evancz/elm-sortable-table#usage-rules) from Elm
language creators. 

## Required boilerplate

The present module contains only convenience functions for working with nested
components in the Elm architecture. A minimal example using this library
with component support can be found
[here](http://github.com/debois/elm-mdl/blob/master/examples/Counter.elm).
We encourage you to use the library in this fashion.

Here is how you use elm-mdl with shorthands. First, boilerplate.

 1. Add a model container for Material components to your model:

        type alias Model =
          { ...
          , mdl : Material.Model
          }

        model : Model =
          { ...
          , mdl = Material.model
          }

 2. Add an action for Material components.

        type Msg =
          ...
          | Mdl (Material.Msg Msg)

 3. Handle that message in your update function as follows:

        update message model =
          case message of
            ...
            Mdl message_ ->
              Material.update Mdl message_ model

 4.  If your app is using Layout and/or Menu, you need also to set up
 subscriptions and initialisations; see `subscriptions` and `init` below.

You now have sufficient boilerplate for using __any__ number of elm-mdl components.
Let's say you need a textfield for name entry, and you'd like to be notifed
whenever the field changes value through your own NameChanged action:

        import Material.Textfield as Textfield
        import Material.Options as Options

        ...

        nameInput : Textfield.Instance Material.Model Msg
        nameInput =

        view addr model =
          ...
          Textfield.render [0] Mdl model.mdl
            [ css "width" "16rem"
            , Textfield.floatingLabel
            , Options.onInput NameChanged
            ]

The win relative to using plain Elm Architecture is that adding a component
neither requires you to update your model, your Msgs, nor your update function.


## Optimising for size

Using this module will force all elm-mdl components to be built and included in
your application. If this is unacceptable, you can custom-build a version of this
module that exposing uses only the components you need. To do so, you need to
provide your own versions of the type aliases `Msg` and `Model` and the value
`model` of the present module.  Use the corresponding definitions in this
module as a starting point
  ([source](https://github.com/debois/elm-mdl/blob/master/src/Material.elm))
  and simply comment out the components you do not need.

## Shorthands

@docs Model, model, Msg, Container, update, update_, subscriptions, init
-}

import Dict
import Material.Component as Component exposing (Indexed, Msg(..))
import Material.Dispatch as Dispatch
import Material.Helpers exposing (map1st)
import Material.Button as Button
import Material.Textfield as Textfield
import Material.Menu as Menu
import Material.Snackbar as Snackbar
import Material.Layout as Layout
import Material.Toggles as Toggles
import Material.Tooltip as Tooltip
import Material.Tabs as Tabs
import Material.Select as Select


{-| Model encompassing all Material components.
-}
type alias Model =
    { button : Indexed Button.Model
    , textfield : Indexed Textfield.Model
    , menu : Indexed Menu.Model
    , snackbar : Maybe (Snackbar.Model Int)
    , layout : Layout.Model
    , toggles : Indexed Toggles.Model
    , tooltip : Indexed Tooltip.Model
    , tabs : Indexed Tabs.Model
    , select : Indexed Select.Model
    }


{-| Initial model.
-}
model : Model
model =
    { button = Dict.empty
    , textfield = Dict.empty
    , menu = Dict.empty
    , snackbar = Nothing
    , layout = Layout.defaultModel
    , toggles = Dict.empty
    , tooltip = Dict.empty
    , tabs = Dict.empty
    , select = Dict.empty
    }


{-| Material message type
TODO: m
-}
type alias Msg m =
    Component.Msg 
        Button.Msg
        Textfield.Msg
        (Menu.Msg m)
        -- Snackbar.Msg
        Layout.Msg
        Toggles.Msg
        Tooltip.Msg
        Tabs.Msg
        (Select.Msg m)
        (List m)



{-| Type of records that have an MDL model container. 
-}
type alias Container c =
    { c | mdl : Model }


{-| Update function for the above Msg. Provide as the first
argument a lifting function that embeds the generic MDL action in
your own Msg type.
-}
update : (Msg m -> m) -> Msg m -> Container c -> ( Container c, Cmd m )
update lift msg container =
  update_ lift msg (.mdl container)
      |> map1st (Maybe.map (\mdl -> { container | mdl = mdl }))
      |> map1st (Maybe.withDefault container)


{-| Variant update function that explicitly signals whether model needs update. 
If it is not clear to you that you need this, you do not :)
-}
update_ : (Msg m -> m) -> Msg m -> Model -> ( Maybe Model, Cmd m )
update_ lift msg store =
    case msg of
       ButtonMsg idx msg ->
           Button.react lift msg idx store

       TextfieldMsg idx msg ->
           Textfield.react lift msg idx store

       MenuMsg idx msg ->
           Menu.react (MenuMsg idx >> lift) msg idx store

       LayoutMsg msg ->
           Layout.react (LayoutMsg >> lift) msg store

       TogglesMsg idx msg ->
           Toggles.react lift msg idx store

       TooltipMsg idx msg ->
           Tooltip.react lift msg idx store

       TabsMsg idx msg ->
           Tabs.react lift msg idx store

       SelectMsg idx msg ->
           Select.react (SelectMsg idx >> lift) msg idx store

       Dispatch msgs -> 
           (Nothing, Dispatch.forward msgs)


{-| Subscriptions and initialisation of elm-mdl. Some components requires
subscriptions in order to function. Hook these up to your containing app as
follows.

    import Material

    type Model =
      { ...
      , mdl : Material.Model
      }

    type Msg =
      ...
      | Mdl (Material.Msg Msg)

    ...

    App.program
      { init = ( model, Material.init Mdl )
      , view = view
      , subscriptions = Material.subscriptions Mdl 
      , update = update
      }

Currently, only Layout and Menu require subscriptions, and only Layout require
initialisation.
-}
subscriptions :
    (Component.Msg button textfield (Menu.Msg m) Layout.Msg toggles tooltip tabs (Select.Msg m) dispatch
     -> m
    )
    -> { model | mdl : Model }
    -> Sub m
subscriptions lift model =
    Sub.batch
        [ Layout.subs lift model.mdl
        , Menu.subs lift model.mdl
        , Select.subs lift model.mdl
        ]


{-| Initialisation. See `subscriptions` above.
-}
init :
    (Component.Msg button textfield menu Layout.Msg toggles tooltip tabs select dispatch -> m)
    -> Cmd m
init lift =
    Layout.sub0 lift
