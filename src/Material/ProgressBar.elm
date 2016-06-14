module Material.ProgressBar exposing
  ( Model, defaultModel, Msg, update, view
  , Property
  , render
  , default, indeterminate, buffering
  , progress, buffer
  )

-- TEMPLATE. Copy this to a file for your component, then update.

{-| From the [Material Design Lite documentation](http://www.getmdl.io/components/#TEMPLATE-section):

> ...

See also the
[Material Design Specification]([https://www.google.com/design/spec/components/TEMPLATE.html).

Refer to [this site](http://debois.github.io/elm-mdl#/template)
for a live demo.

@docs Model, model, Msg, update
@docs view

# Component support

@docs Container, Observer, Instance, instance, fwdTemplate
-}


import Platform.Cmd exposing (Cmd, none)
import Html exposing (..)

import Parts exposing (Indexed)
import Material.Options as Options exposing (Style, cs, nop)


import Html.Attributes exposing (class, classList, style)

-- MODEL


{-| Component model.
-}
type alias Model =
  {
  }


{-| Default component model constructor.
-}
defaultModel : Model
defaultModel =
  {
  }


-- ACTION, UPDATE


{-| Component action.
-}
type Msg
  = NoOp


{-| Component update.
-}
update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
      NoOp -> (model, Cmd.none)


-- PROPERTIES

type BarType
  = Default
  | Indeterminate
  | Buffering


type alias Config =
  { bartype : BarType
  , progress : Float
  , buffer : Float
  }


defaultConfig : Config
defaultConfig =
  { bartype = Default
  , progress = 0
  , buffer = 100
  }


type alias Property m =
  Options.Property Config m


{- See src/Material/Button.elm for an example of, e.g., an onClick handler.
-}

clamp mn mx val =
  max (min mx val) 0

clampSize = clamp 0 100

default : Property m
default =
  Options.set (\config -> { config | bartype = Default })

indeterminate : Property m
indeterminate =
  Options.set (\config -> { config | bartype = Indeterminate })

buffering : Property m
buffering =
  Options.set (\config -> { config | bartype = Buffering })


progress : Float -> Property m
progress amount =
  Options.set (\config -> { config | progress =  clampSize amount})

buffer : Float -> Property m
buffer amount =
  Options.set (\config -> { config | buffer =  clampSize amount})

-- VIEW




{-| Component view.
-}
view : (Msg -> m) -> Model -> List (Property m) -> List (Html m) -> Html m
view lift model options elems =
  let
    summary = Options.collect defaultConfig options
    config = summary.config

  in
    Options.apply summary div
      ([ cs "mdl-progress"
       , cs "mdl-js-progress"
       , cs "is-upgraded"
       , if summary.config.bartype == Indeterminate then
           cs "mdl-progress--indeterminate"
         else
           nop
       ]
        )
        []
        [ div [ classList
                    [ ("progressbar", True)
                    , ("bar", True)
                    , ("bar1", True)
                    ]
              , style [("width", (toString (config.progress) ++ "%"))]
              ] []
        , div [ classList
                    [ ("bufferbar", True)
                    , ("bar", True)
                    , ("bar2", True)
                    ]
              , style [("width", (toString (config.buffer) ++ "%"))]
              ] []
        , div [ classList
                    [ ("auxbar", True)
                    , ("bar", True)
                    , ("bar3", True)
                    ]
              , style [("width", (toString (100 - config.buffer) ++ "%"))]
              ] []
        ]


-- COMPONENT

type alias Container c =
  { c | progressbar : Indexed Model }


{-| Component render.
-}
render
  : (Parts.Msg (Container c) -> m)
  -> Parts.Index
  -> (Container c)
  -> List (Property m)
  -> List (Html m)
  -> Html m
render =
  Parts.create view update .progressbar (\x y -> {y | progressbar=x}) defaultModel

{- See src/Material/Layout.mdl for how to add subscriptions. -}
