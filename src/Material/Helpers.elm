module Material.Helpers
    exposing
        ( filter
        , blurOn
        , map1st
        , map2nd
        , delay
        , cmd
        , pure
        , effect
        , cssTransitionStep
        , lift
        , lift_
        , Update
        , Update_
        , noAttr
        , aria
        )

{-| Convenience functions. These are mostly trivial functions that are used
internally in the library; you might
find some of them useful.

# HTML & Events
@docs filter, blurOn, noAttr, aria

# Cmd
@docs pure, effect, delay, cmd, cssTransitionStep

# Tuples
@docs map1st, map2nd

# Elm architecture
@docs Update, Update_, lift, lift_
-}

import Html
import Html.Attributes
import Platform.Cmd exposing (Cmd)
import Time exposing (Time)
import Task
import Process


{-| Convert a Html element from taking a list of sub-elements to a list of
  Maybe Html. This is convenient if you want to include certain sub-elements
-}
filter : (a -> List b -> c) -> a -> List (Maybe b) -> c
filter elem attr html =
    elem attr (List.filterMap (\x -> x) html)


{-| Add an effect to a value. Example use (supposing you have an
action `MyMsg`):

    model |> effect MyMsg
-}
effect : Cmd b -> a -> ( a, Cmd b )
effect e x =
    ( x, e )


{-| Add the trivial effect to a value. Example use:

    model |> pure
-}
pure : a -> ( a, Cmd b )
pure =
    effect Cmd.none


{-| Attribute which causes element to blur on given event. Example use

    myButton : Html
    myButton =
      button
        [ blurOn "mouseleave" ]
        [ text "Click me!" ]
-}
blurOn : String -> Html.Attribute m
blurOn evt =
    Html.Attributes.attribute ("on" ++ evt) <| "this.blur()"



-- TUPLES


{-| Map the first element of a tuple.

    map1st ((+) 1) (1, "foo") == (2, "foo")
-}
map1st : (a -> c) -> ( a, b ) -> ( c, b )
map1st f ( x, y ) =
    ( f x, y )


{-| Map the second element of a tuple

    map2nd ((+) 1) ("bar", 3) == ("bar", 4)
-}
map2nd : (b -> c) -> ( a, b ) -> ( a, c )
map2nd f ( x, y ) =
    ( x, f y )


{-| Variant of EA update function type, where effects may be
lifted to a different type.
-}
type alias Update_ model action action_ =
    action -> model -> ( model, Cmd action_ )


{-| Standard EA update function type.
-}
type alias Update model action =
    Update_ model action action


{-| Variant of `lift` for effect-free components.
-}
lift_ :
    (model -> submodel)
    -> -- get
       (model -> submodel -> model)
    -> -- set
       (subaction -> submodel -> submodel)
    -> subaction
    -> -- action
       model
    -> -- model
       ( model, Cmd action )
lift_ get set update action model =
    ( set model (update action (get model)), Cmd.none )


{-| Convenience function for writing update-function boilerplate. Example use:

    case msg of
      ...
      ButtonsMsg msg_ ->
        lift .buttons (\m x->{m|buttons=x}) ButtonsMsg Demo.Buttons.update msg_ model

This is equivalent to the more verbose

    case msg of
      ...
      ButtonsMsg msg_ ->
        let
          (buttons_, cmd) =
            Demo.Buttons.update msg_ model.buttons
        in
          ( { model | buttons = buttons_}
          , Cmd.map ButtonsMsg cmd
          )
-}
lift :
    (model -> submodel)
    -> -- get
       (model -> submodel -> model)
    -> -- set
       (subaction -> action)
    -> -- fwd
       Update submodel subaction
    -> -- update
       subaction
    -> -- action
       model
    -> -- model
       ( model, Cmd action )
lift get set fwd update action model =
    let
        ( submodel_, e ) =
            update action (get model)
    in
        ( set model submodel_, Cmd.map fwd e )


{-|
  Lift any value of type `msg` to a `Cmd msg`.
-}
cmd : msg -> Cmd msg
cmd msg =
    Task.perform (always msg) (Task.succeed msg)


{-| Produce a delayed effect. Suppose you want `MyMsg` to happen 200ms after
a button is clicked:

    button
      [ onClick (delay 0.2 MyMsg) ]
      [ text "Click me!" ]
-}
delay : Time -> a -> Cmd a
delay t x =
    Task.perform (always x) <| Process.sleep t


{-| Delay a command sufficiently that you can count on triggering CSS
transitions.
-}
cssTransitionStep : a -> Cmd a
cssTransitionStep x =
    delay 50 x



-- 20 fps


{-| Fake attribute with no effect. Useful to conditionally add attributes, e.g.,

    button
      [ if model.shouldReact then
          onClick ReactToClick
        else
          noAttr
      ]
      [ text "Click me!" ]
-}
noAttr : Html.Attribute a
noAttr =
    Html.Attributes.attribute "data-elm-mdl-noop" ""


{-| Install aria-* attributes, conspicuously missing from elm-lang/html.
-}
aria : String -> Bool -> Html.Attribute a
aria name value =
    if value then
        Html.Attributes.attribute ("aria-" ++ name) "true"
    else
        noAttr
