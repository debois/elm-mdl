module Material.Menu
  ( Model, model
  , Item, item
  , Action, update
  , topLeft
  , bottomLeft
  , topRight
  , bottomRight
  , unaligned
  , view
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

# Style
@docs bottomLeft, bottomRight, topLeft, topRight, unaligned
-}

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

import Material.Menu.Oracle as Oracle exposing (Oracle)
import Material.Ripple as Ripple
import Material.Style as Style exposing (Style, cs, cs', css, css', styled)

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
  { alignment : Style
  , ripple : Bool
  , items : Dict Int Ripple.Model
  , open : Maybe Bool --
  , closing : Bool
  , oracle : Maybe Oracle
  }


{-| Model initialiser. Call with `True` if the menu items should ripple when clicked, `False` otherwise. Pass one of `bottomLeft`, `bottomRight`, `topLeft`, `topRight` to specify the menu's alignment in relation to the button. Use `unaligned` if you want to align it manually.
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
Used with `model`.
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

{-| Item constructor. Pass `True` if there should be a divider below this item; `False` otherwise. Pass as second argument `True` if item should be enabled, and pass as third argument any `Html` as content of the menu item.
-}

item : Bool -> Bool -> Html -> Item
item divider enabled html =
  { html = html
  , enabled = enabled
  , divider = divider
  }


-- ACTION, UPDATE


{-| Component action. The `Select n` action fires when the `n`th Item is selected; `Open`, `Close` when the menu opens, closes.
-}

-- Note: The Close' index is 1-based while the Select index is 0-based.

type Action =
    Toggle Oracle
  | Close' Int Oracle
  | Hide Int Oracle

  | Select Int
  | Open
  | Close

  | Ripple Int Ripple.Action


{-| Component update.
-}
update : Action -> Model -> (Model, Effects Action)
update action model =

  case action of


    -- If the menu is closed, we transition into opening state.
    Toggle oracle -> effect

        (Effects.tick (\_ -> Open))

        { model | open =
                    case model.open of
                      Nothing   -> Just False
                      Just True -> Nothing
                      _         -> model.open

                , oracle =
                    case model.open of
                      Nothing   -> Just oracle
                      Just True -> Just oracle
                      _         -> model.oracle


                , items =
                    [1..List.length oracle.offsetTops]
                    |> List.map (\i -> (i, Ripple.model))
                    |> Dict.fromList
        }

    -- Transition from opening state to open state.
    Open -> pure
      { model | open = if model.open == Just False then
                           Just True
                         else
                           model.open
      }

    -- Immediately close the menu.
    Hide idx oracle -> effect

      ( Effects.tick (\_ -> Select idx) )

      { model | open = Nothing, closing = False, oracle = Just oracle }

    -- Close the menu after some delay.
    Close' idx oracle -> effect

      ( Effects.task
        <| Task.andThen (Task.sleep constant.closeTimeout) << always
        <| Task.succeed (Hide idx oracle))

      { model | closing = True }


    Ripple idx action ->

      let

        (model', effects) = Dict.get idx model.items
                         |> Maybe.withDefault Ripple.model
                         |> Ripple.update action
      in

        effect

          ( Effects.map (Ripple idx) effects )

          { model | items = Dict.insert idx model' model.items }

    Select _ -> pure model
    Close -> pure model


-- VIEW


{-| Component view.
-}
view : Signal.Address Action -> Model -> List Item -> List Html
view addr model items =
  [ styled button
      [ cs "mdl-button"
      , cs "mdl-js-button"
      , cs "mdl-button--icon"
      , Style.attribute (onClick addr Oracle.decode Toggle)
      ]
      [ Html.i [ Html.class "material-icons" ] [ Html.text "more_vert" ]
      ]
  , styled div
      [ cs "mdl-menu__container"
      , cs "is-upgraded"
      , cs' "is-visible" (model.open == Just True)

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

              let
                right e = e.bounds.left + e.bounds.width
              in
                toString (right oracle.container - right oracle.menu)
                ++ "px"

      , flip (css' "bottom") (((model.alignment == topLeft) || (model.alignment == topRight)) && isJust model.oracle) <|

          case model.oracle of
            Nothing -> "auto"
            Just oracle ->

              let
                bottom =
                  oracle.container.bounds.top +
                  oracle.container.bounds.height
              in
                toString (bottom - oracle.button.bounds.top)
                ++ "px"

      , flip (css' "left") (((model.alignment == topLeft) || (model.alignment == bottomLeft)) && isJust model.oracle) <|

          case model.oracle of
            Nothing -> "auto"
            Just oracle ->

              toString oracle.menu.offsetLeft ++ "px"
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
                      |> fromJust "Menu.view: offsetTop"

                offsetHeight n =
                  case model.oracle of
                    Nothing -> 0
                    Just oracle ->
                      oracle.offsetHeights
                      |> List.drop (n-1) -- n is 1-based
                      |> List.head
                      |> fromJust "Menu.view offsetHeight"

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
                    ( (if item.enabled then
                          Style.attribute (onClick addr Oracle.decode' (Close' n))
                        else
                          Style.attribute (Html.attribute "disabled" "disabled"))
                      :: cs "mdl-menu__item"
                      :: css' "transition-delay" itemDelay (isJust model.open)
                      :: cs' "mdl-js-ripple-effect" model.ripple
                      :: cs' "mdl-menu__item--full-bleed-divider" item.divider
                      :: Style.attribute (Html.property "tabindex" (string "-1"))
                      :: []
                    )
--                      Ripple.downOn "mousedown" (Signal.forwardTo addr (Ripple n))
--                      Ripple.downOn "touchstart" (Signal.forwardTo addr (Ripple n))
--                      Ripple.upOn "mouseup" (Signal.forwardTo addr (Ripple n))
--                      Ripple.upOn "mouseleave" (Signal.forwardTo addr (Ripple n))
--                      Ripple.upOn "touchend" (Signal.forwardTo addr (Ripple n))
--                      Ripple.upOn "blur" (Signal.forwardTo addr (Ripple n))

                    ( (::) item.html <|

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


-- HELPER


onClick : Signal.Address Action -> Decoder Oracle -> (Oracle -> Action) -> Attribute
onClick addr decoder action =
  Html.onWithOptions
  "click"
  defaultOptions
  decoder
  (Signal.message addr << action)


isJust : Maybe a -> Bool
isJust x =
  case x of
    Nothing -> False
    Just  _ -> True


fromJust : String -> Maybe a -> a
fromJust crash x =
  case x of
    Nothing -> Debug.crash crash
    Just  x -> x
