module Material.Menu
  ( Model, model
  , Action, update
  , topLeft
  , bottomLeft
  , topRight
  , bottomRight
  , unaligned
  , view
  -- TODO: instnace, fwdTemplate
  -- , instance
  -- , fwdTemplate
  , Container, Observer, Instance
  , item
  ) where

import Effects exposing (Effects, none)
import Html exposing (..)
import Html.Attributes as Html
import Html.Events as Html exposing (defaultOptions)
import Json.Encode exposing (string)
import Json.Decode exposing (Decoder)
import Task
import DOM
import Material.Helpers exposing (..)
import String
import Dict exposing (Dict)

import Material.Component as Component exposing (Indexed)
import Material.Menu.Oracle as Oracle exposing (Oracle)
import Material.Style as Style exposing (Style, cs, cs', css, css', styled)
import Material.Ripple as Ripple

-- CONSTANTS

constant =
  { transitionDurationSeconds  = 0.3
  , transitionDurationFraction = 0.8
  , closeTimeout               = 150
  }

keycodes =
  { enter     = 13
  , escape    = 27
  , space     = 32
  , upArrow   = 38
  , downArrow = 40
  }


-- MODEL


{-| Component model.
-}
type alias Model =
  { alignment : Style
  , ripple : Bool
  , items : Dict Int Ripple.Model
  , open : Maybe Bool
  , closing : Bool
  , oracle : Maybe Oracle
  }


{-| Default component model constructor.
-}
model : Bool -> Style -> Model
model ripple alignemnt =
  { alignment = alignemnt
  , ripple = ripple
  , items = Dict.empty
  , open = Nothing
  , closing = False
  , oracle = Nothing
  }


{-| Component model styles.
-}

bottomLeft : Style
bottomLeft = cs "mdl-menu--bottom-left"

bottomRight : Style
bottomRight = cs "mdl-menu--bottom-right"

topLeft : Style
topLeft = cs "mdl-menu--top-left"

topRight : Style
topRight = cs "mdl-menu--top-right"

unaligned : Style
unaligned = cs "mdl-menu--unaligned"


-- ITEM


{-| Item model.
-}
type alias Item =
  { html    : Html
  , enabled : Bool
  , divider : Bool
  }

item : Bool -> Bool -> Html -> Item
item divider enabled html =
  { html = html
  , enabled = enabled
  , divider = divider
  }


-- ACTION, UPDATE


{-| Component action.
-}
type Action =
    Toggle Oracle
  | Close Int
  | Hide
  | Ripple Int Ripple.Action


{-| Component update.
-}
update : Action -> Model -> (Model, Effects Action)
update action model =

  case action of

    Hide -> pure { model | open = Nothing, closing = False }


    Close idx -> effect

      ( Effects.task
        <| Task.andThen (Task.sleep constant.closeTimeout) << always
        <| Task.succeed Hide)

      { model | closing = True }


    Toggle oracle -> pure

        { model | open =
                    case model.open of
                      Nothing   -> Just True
                      Just True -> Nothing
                      _         -> model.open
                , oracle =
                    case model.open of
                      Nothing -> case model.oracle of
                                   Nothing -> Just oracle
                                   Just oracle -> Just oracle
                      _       -> model.oracle

                , items =
                    [1..List.length oracle.offsetTops]
                    |> List.map (\i -> (i, Ripple.model))
                    |> Dict.fromList
        }


    Ripple idx action ->

      let

        (model', effects) = Dict.get idx model.items
                         |> Maybe.withDefault Ripple.model
                         |> Ripple.update action
      in

        effect

          ( Effects.map (Ripple idx) effects )

          { model | items = Dict.insert idx model' model.items }
    


-- VIEW


{-| Component view.
-}
view : Signal.Address Action -> Model -> List Item -> List Html
view addr model items =
  [ styled button
      [ cs "mdl-button"
      , cs "mdl-js-button"
      , cs "mdl-button--icon"
      ]
      [ onClick' True addr Toggle
      ]
      [ Html.i [ Html.class "material-icons" ] [ Html.text "more_vert" ]
      ]
  , styled div
      [ cs "mdl-menu__container"
      , cs "is-upgraded"
      , cs' "is-visible" (model.open |> Maybe.withDefault False)

      , css' "width"
          ( case model.oracle of
              Just oracle ->
                oracle.menu.bounds.width
                |> toString
                |> flip (++) "px"
              Nothing     -> "auto"
          )
          ( isJust model.oracle )

      , css' "height"
          ( case model.oracle of
              Just oracle ->
                oracle.menu.bounds.height
                |> toString
                |> flip (++) "px"
              Nothing     -> "auto"
          )
          ( isJust model.oracle )

      , flip (css' "top") (((model.alignment == bottomRight) || (model.alignment == bottomLeft)) && (isJust model.oracle)) <|

          case model.oracle of
            Nothing     -> "auto"
            Just oracle ->

              toString (oracle.button.offsetTop + oracle.button.offsetHeight)
              ++ "px"

      , flip (css' "right") (((model.alignment == bottomRight) || (model.alignment == topRight)) && isJust model.oracle) <|
      
          case model.oracle of
            Nothing -> "auto"
            Just oracle ->
            
              toString (oracle.container.bounds.right - oracle.menu.bounds.right)
              ++ "px"

      , flip (css' "bottom") (((model.alignment == topLeft) || (model.alignment == topRight)) && isJust model.oracle) <|
      
          case model.oracle of
            Nothing -> "auto"
            Just oracle ->
            
              toString (oracle.container.bounds.bottom - oracle.button.bounds.top)
              ++ "px"

      , flip (css' "left") (((model.alignment == topLeft) || (model.alignment == bottomLeft)) && isJust model.oracle) <|
      
          case model.oracle of
            Nothing -> "auto"
            Just oracle ->
            
              toString oracle.menu.offsetLeft ++ "px"

      ]
      [
      ]
      [ styled div
          [ cs "mdl-menu__outline"

          , css' "width"
              ( case model.oracle of
                  Just oracle ->
                    oracle.menu.bounds.width
                    |> toString
                    |> flip (++) "px"
                  Nothing     -> "auto"
              )
              ( isJust model.oracle )

          , css' "height"
              ( case model.oracle of
                  Just oracle ->
                    oracle.menu.bounds.height
                    |> toString
                    |> flip (++) "px"
                  Nothing     -> "auto"
              )
              ( isJust model.oracle )

          , model.alignment
          ]
          [
          ]
          [
          ]
      , styled ul
          [ cs "mdl-menu"
          , cs "mdl-js-menu"
          , model.alignment
          , cs' "is-animating" (model.open == Just False)
          , css' "clip"
            (case model.oracle of

               Nothing     -> "auto"
               Just oracle ->

                 let
                   rect x y w h =
                     String.join ""
                     [ "rect( "
                     , toString x ++ "px"
                     , " "
                     , toString y ++ "px"
                     , " "
                     , toString w ++ "px"
                     , " "
                     , toString h ++ "px"
                     , ")"
                     ]

                   width  = oracle.menu.bounds.width
                   height = oracle.menu.bounds.height
                 in
                   if model.open == Just True then

                       rect 0 width height 0

                     else

                       if model.alignment == bottomRight then
                           rect 0 width 0 width
                         else if model.alignment == topLeft then
                           rect height 0 height 0
                         else if model.alignment == topRight then
                           rect height width height width
                         else
                           ""
            )
            (isJust model.oracle)
          ]
          [
          ]
          ( let

              makeItem n item =
                let
                  transitionDuration =
                    constant.transitionDurationSeconds *
                    constant.transitionDurationFraction

                  offsetTop n =
                    case model.oracle of
                      Nothing -> 0
                      Just oracle ->
                        oracle.offsetTops
                        |> List.drop (n-1) -- n is 1-based
                        |> List.head
                        |> \maybe ->
                              case maybe of
                                Just x  -> x
                                Nothing ->
                                  Debug.crash "Menu.view: offsetTop"

                  offsetHeight n =
                    case model.oracle of
                      Nothing -> 0
                      Just oracle ->
                        oracle.offsetHeights
                        |> List.drop (n-1) -- n is 1-based
                        |> List.head
                        |> \maybe ->
                              case maybe of
                                Just x  -> x
                                Nothing ->
                                  Debug.crash "Menu.view: offsetHeight"

                  height =
                    case model.oracle of
                      Nothing -> 0
                      Just oracle -> oracle.menu.bounds.height

                  itemDelay =

                    if model.alignment == topLeft ||
                       model.alignment == topRight then

                        (height - offsetTop n - offsetHeight n) / height * transitionDuration
                        |> toString
                        |> flip (++) "s"

                      else

                        ((offsetTop n) / height * transitionDuration)
                        |> toString
                        |> (flip (++) "s")
                in

                      styled li
                        [ cs "mdl-menu__item"
                        , css' "transition-delay" itemDelay
                            (isJust model.open)
                        , cs' "mdl-js-ripple-effect" model.ripple
                        , cs' "mdl-menu__item--full-bleed-divider" item.divider
                        ]
                        ( [ Html.property "tabindex" (string "-1")
                          , Ripple.downOn "mousedown" (Signal.forwardTo addr (Ripple n))
                          , Ripple.downOn "touchstart" (Signal.forwardTo addr (Ripple n))
                          , Ripple.upOn "mouseup" (Signal.forwardTo addr (Ripple n))
                          , Ripple.upOn "mouseleave" (Signal.forwardTo addr (Ripple n))
                          , Ripple.upOn "touchend" (Signal.forwardTo addr (Ripple n))
                          , Ripple.upOn "blur" (Signal.forwardTo addr (Ripple n))
                          ] ++
                          if item.enabled then
                              [ Html.onClick addr (Close n) ]
                            else
                              [ Html.attribute "disabled" "disabled" ]
                        )
                        (

                          (::) item.html <|

                            if model.ripple then

                                [ Ripple.view
                                  ( Signal.forwardTo addr (Ripple n))
                                  [ Html.class "mdl-menu__item-ripple-container" ]
                                  ( Dict.get n model.items
                                    |> Maybe.withDefault Ripple.model)
                                ]

                              else
                                []
                        )

            in
              List.map2 makeItem [1..List.length items] items
          )
      ]
  ]


-- COMPONENT


{-|
-}
type alias Container c =
  { c | template : Indexed Model }


{-|
-}
type alias Observer obs =
  Component.Observer Action obs


{-|
-}
type alias Instance container obs =
  Component.Instance
    Model container Action obs (List Style -> Html)


{-| Create a component instance. Example usage, assuming you have a type
`Action` with a constructor ...
-}
--instance :
--  Int
--  -> (Component.Action (Container c) obs -> obs)
--  -> Model
--  -> List (Observer obs)
--  -> Instance (Container c) obs
--
--instance id lift model0 observers =
--  Component.instance
--    view update .template (\x y -> {y | template = x}) id lift model0 observers


{-|
-}
--fwdTemplate : obs -> (Observer obs)
--fwdTemplate obs action =
--  case action of
--    Toggle -> Just obs

--

onClick preventDefault addr action =
  Html.onWithOptions
    "click"
    { defaultOptions | preventDefault = preventDefault }
    Html.targetValue
    (\_ -> Signal.message addr action)

onClick' preventDefault addr action =
  Html.onWithOptions
    "click"
    { defaultOptions | preventDefault = preventDefault }
    Oracle.decode
    (Signal.message addr << action)

onKeyDown preventDefault addr action =
  Html.onWithOptions
    "click"
    { defaultOptions | preventDefault = preventDefault }
    Html.keyCode
    (Signal.message addr << action)

onShow preventDefault addr action =
  Html.onWithOptions
    "load"
    defaultOptions
    Html.targetValue
    (\_ -> Signal.message addr action)

isJust x =
  case x of
    Nothing -> False
    Just  _ -> True

fromJust crash x =
  case x of
    Nothing -> Debug.crash crash
    Just  x -> x
