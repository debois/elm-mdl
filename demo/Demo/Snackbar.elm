module Demo.Snackbar where

import Effects exposing (Effects, none)
import Html exposing (..)
import Html.Attributes exposing (class, style, key)
import Array exposing (Array)
import Time exposing (Time, millisecond)

import Material.Helpers exposing (map1st, map2nd, pure, delay)
import Material.Color as Color
import Material.Style as Style exposing (cs, css, Style)
import Material.Snackbar as Snackbar
import Material.Button as Button exposing (Action(..))
import Material.Grid exposing (..)
import Material.Elevation exposing (e2, e8)
import Material 

import Demo.Page as Page



-- MODEL



type alias Mdl = 
  Material.Model 


type Square' 
  = Appearing 
  | Waiting
  | Active
  | Idle
  | Disappearing


type alias Square = 
  (Int, Square')


type alias Model =
  { count : Int
  , squares : List Square
  , snackbar : Snackbar.Model Int
  , mdl : Mdl
  }


model : Model
model =
  { count = 0
  , squares = [] 
  , snackbar = Snackbar.model 
  , mdl = Material.model
  }


-- ACTION, UPDATE


type Action
  = AddSnackbar
  | AddToast
  | Appear Int
  | Gone Int
  | Snackbar (Snackbar.Action Int)
  | MDL (Material.Action Action)


add : (Int -> Snackbar.Contents Int) -> Model -> (Model, Effects Action)
add f model =
  let 
    (snackbar', fx) = 
      Snackbar.add (f model.count) model.snackbar
        |> map2nd (Effects.map Snackbar)
    model' = 
      { model 
      | snackbar = snackbar'
      , count = model.count + 1
      , squares = (model.count, Appearing) :: model.squares
      }
  in 
    ( model'
    , Effects.batch 
        [ Effects.tick (always (Appear model.count))
        , fx 
        ]
    )


mapSquare : Int -> (Square' -> Square') -> Model -> Model
mapSquare k f model = 
  { model 
  | squares = 
      List.map 
        ( \((k', sq) as s) -> if k /= k' then s else (k', f sq) )
        model.squares
  }


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    AddSnackbar ->
      add (\k -> Snackbar.snackbar k ("Snackbar message #" ++ toString k) "UNDO") model

    AddToast -> 
      add (\k -> Snackbar.toast k <| "Toast message #" ++ toString k) model

    Appear k -> 
      model |> mapSquare k (\sq -> if sq == Appearing then Waiting else sq) |> pure

    Snackbar (Snackbar.Begin k) -> 
      model |> mapSquare k (always Active) |> pure

    Snackbar (Snackbar.End k) -> 
      model |> mapSquare k (always Idle) |> pure

    Snackbar (Snackbar.Click k) -> 
      ( model |> mapSquare k (always Disappearing)
      , delay transitionLength (Gone k) 
      )

    Gone k ->
      ({ model
       | squares = List.filter (fst >> (/=) k) model.squares
       }
      , none)

    Snackbar action' -> 
      Snackbar.update action' model.snackbar 
        |> map1st (\s -> { model | snackbar = s })
        |> map2nd (Effects.map Snackbar)

    MDL action' -> 
      Material.update MDL action' model.mdl
        |> map1st (\m -> { model | mdl = m })



-- VIEW



addSnackbarButton : Button.Instance Mdl Action 
addSnackbarButton = 
  Button.instance 0 MDL
    Button.raised (Button.model True)
    [ Button.fwdClick AddSnackbar ]


addToastButton : Button.Instance Mdl Action 
addToastButton = 
  Button.instance 1 MDL
    Button.raised (Button.model True)
    [ Button.fwdClick AddToast ]


boxHeight : String
boxHeight = "48px"


boxWidth : String
boxWidth = "64px"


transitionLength : Time 
transitionLength = 150 * millisecond


transitionInner : Style
transitionInner = 
  css "transition"
    <| "box-shadow 333ms ease-in-out 0s, " 
    ++ "width " ++ toString transitionLength ++ "ms, " 
    ++ "height " ++ toString transitionLength ++ "ms, "
    ++ "background-color " ++ toString transitionLength ++ "ms"


transitionOuter : Style
transitionOuter = 
  css "transition" 
    <| "width " ++ toString transitionLength ++ "ms ease-in-out 0s, "
    ++ "margin " ++ toString transitionLength ++ "ms ease-in-out 0s"


clickView : Model -> Square -> Html
clickView model (k, square) =
  let
    hue =
      Array.get ((k + 4) % Array.length Color.hues) Color.hues
        |> Maybe.withDefault Color.Teal

    shade = 
      case square of
        Idle -> 
          Color.S100

        _ -> 
          Color.S500

    color = 
      Color.color hue shade

    selected' =
      square == Active

    (width, height, margin, selected) = 
      if square == Appearing || square == Disappearing then
        ("0", "0", "16px 0", False)
      else
        (boxWidth, boxHeight, "16px 16px", selected')      

  in
    {- In order to get the box appearance and disappearance animations 
    to start in the lower-left corner, we render boxes as an outer div 
    (which animates only width, to cause reflow of surrounding boxes), 
    and an absolutely positioned inner div (to force animation to start
    in the lower-left corner. -}
    Style.div 
      [ css "height" boxHeight
      , css "width" width
      , css "position" "relative"
      , css "display" "inline-block"
      , css "margin" margin
      , css "z-index" "0"
      , transitionOuter
      , Style.attribute (key <| toString k)
        {- Interestingly, not setting key messes up CSS transitions in
        spectacular ways. -}
      ]
      [ Style.div
          [ Color.background color
          , Color.text Color.primaryContrast
          , if selected then e8 else e2
            -- Center contents
          , css "display" "inline-flex"
          , css "align-items" "center"
          , css "justify-content" "center"
          , css "flex" "0 0 auto"
             -- Sizing
          , css "height" height
          , css "width" width
          , css "border-radius" "2px"
          , css "box-sizing" "border-box"
             -- Force appearance/disapparenace to be from/to lower-left corner. 
          , css "position" "absolute"
          , css "bottom" "0"
          , css "left" "0"
             -- Transitions
          ,  transitionInner
          ]
          [ div [] [ text <| toString k ] ]
        ]



view : Signal.Address Action -> Model -> Html
view addr model =
  Page.body2 "Snackbar & Toast" srcUrl intro references
    [ p [] 
        [ text """Click the buttons below to generate toasts and snackbars. Note that 
                  multiple activations are automatically queued."""
        ]
    , grid [ ] 
        [ cell 
            [ size All 4, size Desktop 2]
            [ addSnackbarButton.view addr model.mdl 
                [ Button.colored
                , css "width" "8em"
                ] 
                [ text "Snackbar" ]
            ]
        , cell 
            [ size All 4, size Desktop 2]
            [ addToastButton.view addr model.mdl 
                [ Button.colored
                , css "width" "8em"
                ] 
                [ text "Toast" ]
            ]
        , cell
            [ size Desktop 10, offset Desktop 1 
            , size Tablet 6, offset Tablet 1
            , size Phone 4
            , align Top 
            , css "padding-top" "32px"
            ]
            (model.squares |> List.reverse |> List.map (clickView model))
        ]
    , Snackbar.view (Signal.forwardTo addr Snackbar) model.snackbar 
    ]


intro : Html
intro = 
  Page.fromMDL "https://www.getmdl.io/components/index.html#snackbar-section" """
> The Material Design Lite (MDL) __snackbar__ component is a container used to
> notify a user of an operation's status. It displays at the bottom of the
> screen. A snackbar may contain an action button to execute a command for the
> user. Actions should undo the committed action or retry it if it failed for
> example. Actions should not be to close the snackbar. By not providing an
> action, the snackbar becomes a __toast__ component.

""" 


srcUrl : String 
srcUrl =
  "https://github.com/debois/elm-mdl/blob/master/demo/Demo/Snackbar.elm"


references : List (String, String)
references = 
  [ Page.package "http://package.elm-lang.org/packages/debois/elm-mdl/latest/Material-Snackbar"
  , Page.mds "https://www.google.com/design/spec/components/snackbars-toasts.html"
  , Page.mdl "https://www.getmdl.io/components/index.html#snackbar-section"
  ]


