module Material.Menu
  ( Model, model, Alignment(..)
  , Item, item
  , Action, update
  , view
  , Instance, Container, Observer
  , instance
  , fwdOpen, fwdClose, fwdSelect
  ) where

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
[this site](https://debois.github.io/elm-mdl/#/menus)
for a live demo.

# Elm architecture
@docs Model, model, Action, update, View

# Alignment
@docs BottomLeft, BottomRight, TopLeft, TopRight, Unaligned
-}

import Array exposing (Array)
import Dict exposing (Dict)
import Effects exposing (Effects, none)
import Html.Attributes as Html exposing (..)
import Html.Events as Html exposing (defaultOptions)
import Html exposing (..)
import Json.Decode as Json exposing (Decoder)
import Json.Encode exposing (string)
import Material.Helpers exposing (..)
import String
import Task

import Material.Menu.Geometry as Geometry exposing (Geometry)
import Material.Ripple as Ripple
import Material.Style as Style exposing (Style, cs, cs', css, css', styled)
import Material.Component as Component exposing (Indexed)

{-| MDL menu.
-}


-- CONSTANTS


constant :
  { transitionDurationSeconds  : Float
  , transitionDurationFraction : Float
  , closeTimeout               : Float
  }
constant =
  { transitionDurationSeconds  = 0.3
  , transitionDurationFraction = 0.8
  , closeTimeout               = 150
  }


-- TODO: Key codes are not implemented yet.


--keycodes =
--  { enter     = 13
--  , escape    = 27
--  , space     = 32
--  , upArrow   = 38
--  , downArrow = 40
--  }


-- MODEL


{-| Model of the menu; common to all kinds of menus.
Use `model` to initialise it.
-}
type alias Model =
  { alignment : Alignment
  , ripple : Bool
  , items : Dict Int Ripple.Model
  , animationState : AnimationState
  , geometry : Maybe Geometry
  }


type AnimationState
  = Idle
  | Opening
  | Opened
  | Closing


{-| Model initialiser. Call with `True` if the menu items should ripple when clicked, `False` otherwise.
-}
model : Bool -> Alignment -> Model
model ripple alignemnt =
  { alignment = alignemnt
  , ripple = ripple
  , items = Dict.empty
  , animationState = Idle
  , geometry = Nothing
  }


{-| Menu alignment.
Used with `model`. This specifies where the menu opens in relation to the
button, rather than where the menu is positioned.
-}
type Alignment =
    BottomLeft
  | BottomRight
  | TopLeft
  | TopRight
  | Unaligned


-- ITEM


{-| Item model.
-}
type alias Item =
  { html    : Html
  , enabled : Bool
  , divider : Bool
  }


{-| Item constructor. Pass `True` if there should be a divider below this item; `False` otherwise. Pass as second argument `True` if item should be enabled, and pass as third argument any `Html` as content of the menu item.
-}
item : Bool -> Bool -> Html -> Item
item divider enabled html =
  { html = html
  , enabled = enabled
  , divider = divider
  }


-- ACTION, UPDATE


{-| Component action.
-}

-- Note: The Close' index is 1-based while the Select index is 0-based.

type Action
  = Open' Geometry
  | Select' Int Geometry
  | Close' Geometry
  | Tick'
  | Hide' Int Geometry
  | Ripple' Int Ripple.Action

  -- public actions:

  | Open
  | Select Int
  | Close


{-| Component update.
-}
update : Action -> Model -> (Model, Effects Action)
update action model =

  case action of

    -- Open the menu.

    Open' geometry ->

      effect
      ( Effects.task (Task.succeed Tick') )
      { model | animationState =
                  case model.animationState of
                    Opened  -> Opened
                    _       -> Opening

              , geometry = Just geometry
              , items =
                  [1..Array.length geometry.offsetTops]
                  |> List.map (\i -> (i, Ripple.model))
                  |> Dict.fromList
      }

    -- Transition to `Opened` animation state.

    Tick' ->
      effect
      ( Effects.task (Task.succeed Open) )
      { model | animationState = Opened
      }

    -- Immediately close the menu.

    Close' geometry ->

      effect
      ( Effects.task (Task.succeed Close) )
      { model | animationState = Idle
              , geometry = Just geometry
      }

    -- Immediately close the menu (but propagate `Select n` instead of
    -- `Close`).

    Hide' idx geometry ->

      effect
      ( Effects.task (Task.succeed (Select idx)) )
      { model | animationState = Idle
              , geometry = Just geometry
      }

    -- Close the menu after some delay for the ripple effect to show.

    Select' idx geometry ->

      effect
      ( Effects.task
        <| Task.andThen (Task.sleep constant.closeTimeout) << always
        <| Task.succeed (Hide' idx geometry) )

      { model | animationState = Closing }


    -- Update `Ripple` component.

    Ripple' idx action ->

      let

        (model', effects) = Dict.get idx model.items
                         |> Maybe.withDefault Ripple.model
                         |> Ripple.update action
      in

        effect

          ( Effects.map (Ripple' idx) effects )

          { model | items = Dict.insert idx model' model.items }

    Open     -> pure model
    Close    -> pure model
    Select _ -> pure model


-- VIEW


{-| Component view.
-}
view : Signal.Address Action -> Model -> List Item -> List Html
view addr model items =
  [ styled button
    [ cs "mdl-button"
    , cs "mdl-js-button"
    , cs "mdl-button--icon"
    , Style.attribute
      (case model.animationState of
         Opened  -> onClick addr Geometry.decode Close'
         _       -> onClick addr Geometry.decode Open'
      )
    ]
    [ styled i
      [ cs "material-icons"
      , Style.attribute (Html.style [("pointer-events", "none")])
      ]
      [ text "more_vert"
      ]
    ]
  , styled div
    [ cs "mdl-menu__container"
    , cs "is-upgraded"
    , cs' "is-visible" ((model.animationState == Opened) || (model.animationState == Closing))

    , css' "width"
        ( case model.geometry of
            Just geometry -> geometry.menu.bounds.width |> toPx
            Nothing -> "auto"
        )
        ( model.geometry /= Nothing )

    , css' "height"
        ( case model.geometry of
            Just geometry -> geometry.menu.bounds.height |> toPx
            Nothing -> "auto"
        )
        ( model.geometry /= Nothing )

    , flip (css' "top") (((model.alignment == BottomRight) || (model.alignment == BottomLeft)) && (model.geometry /= Nothing)) <|

        case model.geometry of
          Nothing -> "auto"
          Just geometry ->
            (geometry.button.offsetTop + geometry.button.offsetHeight)
            |> toPx

    , flip (css' "right") (((model.alignment == BottomRight) || (model.alignment == TopRight)) && model.geometry /= Nothing) <|

        case model.geometry of
          Nothing -> "auto"
          Just geometry ->
            let
              right e = e.bounds.left + e.bounds.width
            in
              (right geometry.container - right geometry.menu)
              |> toPx

    , flip (css' "bottom") (((model.alignment == TopLeft) || (model.alignment == TopRight)) && model.geometry /= Nothing) <|

        case model.geometry of
          Nothing -> "auto"
          Just geometry ->
            let
              bottom =
                geometry.container.bounds.top +
                geometry.container.bounds.height
            in
              (bottom - geometry.button.bounds.top) |> toPx

    , flip (css' "left") (((model.alignment == TopLeft) || (model.alignment == BottomLeft)) && model.geometry /= Nothing) <|

        case model.geometry of
          Just geometry -> geometry.menu.offsetLeft |> toPx
          Nothing -> "auto"
    ]
    [ styled div
      [ cs "mdl-menu__outline"

      , css' "width"
          ( case model.geometry of
              Nothing -> "auto"
              Just geometry -> geometry.menu.bounds.width |> toPx
          )
          (model.geometry /= Nothing)

      , css' "height"
          ( case model.geometry of
              Just geometry -> geometry.menu.bounds.height |> toPx
              Nothing -> "auto"
          )
          (model.geometry /= Nothing)

      , case model.alignment of
          BottomLeft  -> cs "mdl-menu--bottom-left"
          BottomRight -> cs "mdl-menu--bottom-right"
          TopLeft     -> cs "mdl-menu--top-left"
          TopRight    -> cs "mdl-menu--top-right"
          Unaligned   -> cs "mdl-menu--unaligned"
      ]
      [
      ]
    , styled ul
      [ cs "mdl-menu"
      , cs "mdl-js-menu"
      , case model.alignment of
          BottomLeft -> cs "mdl-menu--bottom-left"
          BottomRight -> cs "mdl-menu--bottom-right"
          TopLeft -> cs "mdl-menu--top-left"
          TopRight -> cs "mdl-menu--top-right"
          Unaligned -> cs "mdl-menu--unaligned"
      , cs' "is-animating" ((model.animationState == Opening) || (model.animationState == Closing))
      , css' "clip"
        (case model.geometry of
           Nothing -> "auto"
           Just geometry ->
             let
               width  = geometry.menu.bounds.width
               height = geometry.menu.bounds.height
             in
               if (model.animationState == Opened) || (model.animationState == Closing) then

                   rect 0 width height 0

                 else

                   case model.alignment of
                     BottomRight -> rect 0 width 0 width
                     TopLeft -> rect height 0 height 0
                     TopRight -> rect height width height width
                     _ -> ""
        )
        (model.geometry /= Nothing)
      ]
      (List.map2 (makeItem addr model) [1..List.length items] items)
    ]
  ]

makeItem : Signal.Address Action -> Model -> Int -> Item -> Html
makeItem addr model n item =
  let
    transitionDuration =
      constant.transitionDurationSeconds *
      constant.transitionDurationFraction

    offsetTop n =
      case model.geometry of
        Nothing -> 0
        Just geometry ->
          geometry.offsetTops
          |> Array.get (n-1) -- n is 1-based
          |> Maybe.withDefault 0

    offsetHeight n =
      case model.geometry of
        Nothing -> 0
        Just geometry ->
          geometry.offsetHeights
          |> Array.get (n-1) -- n is 1-based
          |> Maybe.withDefault 0

    height =
      case model.geometry of
        Nothing -> 0
        Just geometry -> geometry.menu.bounds.height

    itemDelay =

      if model.alignment == TopLeft ||
         model.alignment == TopRight then

          (height - offsetTop n - offsetHeight n) / height * transitionDuration
          |> toString
          |> flip (++) "s"

        else

          ((offsetTop n) / height * transitionDuration)
          |> toString
          |> (flip (++) "s")
  in

        styled li
        [ if item.enabled then
             Style.attribute (onClick addr Geometry.decode' (Select' n))
           else
             Style.attribute (Html.attribute "disabled" "disabled")
        , cs "mdl-menu__item"
        , css' "transition-delay" itemDelay ((model.animationState == Opening) || (model.animationState == Opened))
        , cs' "mdl-js-ripple-effect" model.ripple
        , cs' "mdl-menu__item--full-bleed-divider" item.divider
        , Style.attribute (Html.property "tabindex" (string "-1"))
        , Style.attribute (Ripple.downOn "mousedown" (Signal.forwardTo addr (Ripple' n)))
        , Style.attribute (Ripple.downOn "touchstart" (Signal.forwardTo addr (Ripple' n)))
        , Style.attribute (Ripple.upOn "mouseup" (Signal.forwardTo addr (Ripple' n)))
        , Style.attribute (Ripple.upOn "mouseleave" (Signal.forwardTo addr (Ripple' n)))
        , Style.attribute (Ripple.upOn "touchend" (Signal.forwardTo addr (Ripple' n)))
        , Style.attribute (Ripple.upOn "blur" (Signal.forwardTo addr (Ripple' n)))
        ]

        ( if model.ripple then

              [ item.html
              , Ripple.view
                ( Signal.forwardTo addr (Ripple' n))
                [ Html.class "mdl-menu__item-ripple-container" ]
                ( Dict.get n model.items
                  |> Maybe.withDefault Ripple.model)
              ]

            else
              [ item.html ]
        )


-- COMPONENT


type alias Container c =
  { c | menu : Indexed Model }


type alias Observer obs =
  Component.Observer Action obs


type alias Instance container obs =
  Component.Instance Model container Action obs (List Item -> List Html)


instance :
  Int
  -> (Component.Action (Container c) obs -> obs)
  -> Model
  -> List (Observer obs)
  -> Instance (Container c) obs


instance id lift model0 observer =
  Component.instance
    view update .menu (\x y -> {y | menu = x}) id lift model0 observer


fwdOpen : obs -> Observer obs
fwdOpen obs action =
  case action of
    Open -> Just obs
    _ -> Nothing


fwdClose : obs -> Observer obs
fwdClose obs action =
  case action of
    Close -> Just obs
    _ -> Nothing


fwdSelect : obs -> Observer obs
fwdSelect obs action =
  case action of
    Select _ -> Just obs
    _ -> Nothing


-- HELPER


onClick :
  Signal.Address Action
  -> Decoder Geometry
  -> (Geometry -> Action)
  -> Attribute

onClick addr decoder action =
  Html.onWithOptions
  "click"
  defaultOptions
  decoder
  (Signal.message addr << action)

rect : Float -> Float -> Float -> Float -> String
rect x y w h =
 [x,y,w,h]
 |> List.map toPx
 |> String.join " "
 |> (\coords -> "rect(" ++ coords ++ ")")

toPx : Float -> String
toPx = toString >> flip (++) "px"
