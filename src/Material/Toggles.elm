module Material.Toggles
  ( Model, model
  , Action, update
  , switch, checkbox, radio
  , instance, fwdChange
  , Container, Observer, Instance
  , Radio, Checkbox, Switch
  ) where

{-| From the [Material Design Lite documentation](http://www.getmdl.io/index.html#toggles-section/checkbox):

> The Material Design Lite (MDL) checkbox component is an enhanced version of the
> standard HTML `<input type="checkbox">` element. A checkbox consists of a small
> square and, typically, text that clearly communicates a binary condition that
> will be set or unset when the user clicks or touches it. Checkboxes typically,
> but not necessarily, appear in groups, and can be selected and deselected
> individually. The MDL checkbox component allows you to add display and click
>     effects.
> 
> Checkboxes are a common feature of most user interfaces, regardless of a site's
> content or function. Their design and use is therefore an important factor in
> the overall user experience. [...]
> 
> The enhanced checkbox component has a more vivid visual look than a standard
> checkbox, and may be initially or programmatically disabled.

See also the
[Material Design Specification](https://www.google.com/design/spec/components/selection-controls.html#).

Refer to [this site](http://debois.github.io/elm-mdl#/toggles)
for a live demo.

@docs Model, model, Action, update
@docs view

# Component support

@docs Container, Observer, Instance, instance, fwdTemplate
-}


import Effects exposing (Effects, none)
import Html exposing (..)
import Html.Attributes exposing (type', class, disabled, checked)
import Html.Events exposing (on, onFocus, onBlur)
import Json.Decode as Decode
import Signal exposing (Address, forwardTo, message)

import Material.Component as Component exposing (Indexed)
import Material.Style as Style exposing (Style, cs, cs', styled, attribute, multiple)
import Material.Helpers exposing (map1st, map2nd, blurOn, filter)
import Material.Ripple as Ripple



-- MODEL


type State = S Ripple.Model


{-| Component model.
-}
type alias Model =
  { isFocused : Bool
  , isDisabled : Bool
  , value : Bool
  , ripple : Bool
  , state : State
  }


{-| Default component model constructor.
-}
model : Model
model =
  { isFocused = False
  , isDisabled = False
  , value = False
  , ripple = True
  , state = S Ripple.model
  }


state : Model -> Ripple.Model
state model = 
  case model.state of 
    S ripple -> 
      ripple

-- ACTION, UPDATE


{-| Component action.
-}
type Action
  = Change
  | Ripple Ripple.Action
  | SetFocus Bool


{-| Component update.
-}
update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of 
    Change -> 
      ( { model | value = not model.value }, none )

    Ripple rip -> 
      Ripple.update rip (state model)
        |> map1st (\r -> { model | state = S r })
        |> map2nd (Effects.map Ripple)

    SetFocus focus -> 
      ( { model | isFocused = focus }, none )



-- VIEW



top : String -> Address Action -> Model -> List Style -> List Html -> Html
top name addr model styles elems = 
  styled label 
    [ cs ("mdl-" ++ name) 
    , cs ("mdl-js-" ++ name)
    , cs' "mdl-js-ripple-effect" model.ripple
    , cs' "mdl-js-ripple-effect--ignore-events" model.ripple
    , cs "is-upgraded"
    , cs' "is-checked" model.value
    , attribute <| on "change" Decode.value (always (message addr Change))
    , attribute <| blurOn "mouseup"
    , attribute <| onFocus addr (SetFocus True)
    , attribute <| onBlur addr (SetFocus False)
    , multiple styles
    ] 
    (if model.ripple then 
       (Ripple.view 
         ( forwardTo addr Ripple)
         [ class "mdl-switch__ripple-container mdl-js-ripple-effect mdl-ripple--center" ]
         (state model)
       ) :: elems
     else
       elems)



checkbox : Address Action -> Model -> List Style -> Html
checkbox addr model styles = 
  [ input
    [ type' "checkbox"
    , class ("mdl-checkbox__input")
    , disabled model.isDisabled
    , checked model.value 
      {- TODO: the checked attribute is not rendered. Switch still seems to
      work, though, but accessibility is probably compromised. 
      https://github.com/evancz/elm-html/issues/91
      -}
    ]
    []
  , span [ class ("mdl-checkbox__label") ] [] 
  , span [ class "mdl-checkbox__focus-helper" ] [] 
  , span 
      [ class "mdl-checkbox__box-outline" ]
      [ span 
          [ class "mdl-checkbox__tick-outline" ]
          []
      ]
  ]
  |> top "checkbox" addr model styles


{-| TODO
-}
switch : Address Action -> Model -> List Style -> Html
switch addr model styles =
  [ input
    [ type' "checkbox"
    , class "mdl-switch__input"
    , disabled model.isDisabled
    , checked model.value 
      {- TODO: the checked attribute is not rendered. Switch still seems to
      work, though, but accessibility is probably compromised. 
      https://github.com/evancz/elm-html/issues/91
      -}
    ]
    []
  ,  span [ class "mdl-switch__label" ] []
  ,  div [ class "mdl-switch__track" ] []
  ,  div 
       [ class "mdl-switch__thumb" ] 
       [ span [ class "mdl-switch__focus-helper" ] [] ]
  ]
  |> top "switch" addr model styles


type alias RadioId = 
  (String, String)


radio : Address Action -> Model -> List Style -> RadioId -> List Html -> Html
radio addr model styles (value, name) elems = 
  [ input 
    [ type' "radio"
    , class "mdl-radio__button"
    , disabled model.isDisabled
    , checked model.value
    , Html.Attributes.value value
    , Html.Attributes.name name
    ] 
    []
  , span [ class "mdl-radio__label" ] elems
  , span [ class "mdl-radio__outer-circle" ] [] 
  , span [ class "mdl-radio__inner-circle" ] [] 
  ]
  |> top "radio" addr model styles


-- COMPONENT


{-|
-}
type alias View a =
  Address Action -> Model -> List Style -> a


{-| 
-}
type alias Container c =
  { c | toggles : Indexed Model }


{-|
-}
type alias Observer obs = 
  Component.Observer Action obs


{-|
-}
type alias Instance container obs v =
  Component.Instance 
    Model container Action obs (List Style -> v)

type alias Radio container obs = 
  Instance container obs (RadioId -> List Html -> Html)

type alias Checkbox container obs = 
  Instance container obs Html

type alias Switch container obs = 
  Instance container obs Html


{-| Create a component instance. Example usage, assuming you have a type
`Action` with a constructor ...
-}
instance : 
  Int
  -> (Component.Action (Container c) obs -> obs)
  -> (View v)
  -> Model
  -> List (Observer obs)
  -> Instance (Container c) obs v

instance id lift view model0 observers = 
  Component.instance 
    view update .toggles (\x y -> {y | toggles = x}) id lift model0 observers

{-| 
-}
fwdChange : obs -> (Observer obs)
fwdChange obs action = 
  case action of 
    Change -> Just obs
    _ -> Nothing
    


