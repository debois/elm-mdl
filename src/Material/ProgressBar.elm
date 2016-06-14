module Material.ProgressBar exposing
  ( view
  , Property
  , BarType(Default, Indeterminate)
  , default, indeterminate
  , progress, buffer
  )

{-| From the [Material Design Lite documentation](https://getmdl.io/components/index.html#loading-section/progress):

> The Material Design Lite (MDL) progress component is a visual indicator of
> background activity in a web page or application. A progress indicator
> consists of a (typically) horizontal bar containing some animation that
> conveys a sense of motion. While some progress devices indicate an approximate
> or specific percentage of completion, the MDL progress component simply
> communicates the fact that an activity is ongoing and is not yet complete.

> Progress indicators are an established but non-standardized feature in user
> interfaces, and provide users with a visual clue to an application's status.
> Their design and use is therefore an important factor in the overall user
> experience. See the progress component's Material Design specifications page for
> details.

See also the
[Material Design Specification](https://material.google.com/components/progress-activity.html).

Refer to [this site](http://debois.github.io/elm-mdl#/progressbar)
for a live demo.

# Options
@docs Property

# Render
@docs view

# Type
@docs BarType
@docs default, indeterminate

# Appearence
@docs progress, buffer

-}


import Platform.Cmd exposing (Cmd, none)
import Html exposing (..)

import Parts exposing (Indexed)
import Material.Options as Options exposing (Style, cs, nop)


import Html.Attributes exposing (class, classList, style)


-- PROPERTIES

{-| The type of the progress bar.

`Default` bars require the user to manually upgrade the the progress using `progress`.

`Indeterminate` bars use CSS animations.
-}
type BarType
  = Default
  | Indeterminate


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


{-| Properties for Progress bar options.
-}
type alias Property m =
  Options.Property Config m

-- Helper functions

clamp : number -> number -> number -> number
clamp mn mx val =
  max (min mx val) 0

clampSize : number -> number
clampSize = clamp 0 100

-- ATTRIBUTES

{-| Set the type of the progress bar to 'default'.
-}
default : Property m
default =
  Options.set (\config -> { config | bartype = Default })

{-| Set the type of the progress bar to 'indeterminate'.

Indeterminate progress bars show an animated indicator
-}
indeterminate : Property m
indeterminate =
  Options.set (\config -> { config | bartype = Indeterminate })


{-| Set the current progress of the progressbar.
-}
progress : Float -> Property m
progress amount =
  Options.set (\config -> { config | progress =  clampSize amount})

{-| Set the current progress of the buffer.
-}
buffer : Float -> Property m
buffer amount =
  Options.set (\config -> { config | buffer =  clampSize amount})

-- VIEW


{-| View function for progress bars. Set the progress of the bar with `progress`,
you can also set the buffer of the bar using `buffer`. You can also use an indeterminate
progress bar by using `indeterminate`.

For example:

    import Material.ProgressBar as ProgressBar

    progressBar : Html m
    progressBar = ProgressBar.view [ ProgressBar.progress 35
                                   , ProgressBar.buffer 85]
-}
view : List (Property m) -> Html m
view options =
  let
    summary = Options.collect defaultConfig options
    config = summary.config

  in
    Options.apply summary div
      [ cs "mdl-progress"
      , cs "mdl-js-progress"
      , cs "is-upgraded"
      , if summary.config.bartype == Indeterminate then
          cs "mdl-progress--indeterminate"
        else
          nop
      ]
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
