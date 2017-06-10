module Material.Slider
    exposing
        ( Property
        , value
        , min
        , max
        , disabled
        , step
        , view
        , onChange
        )

{-| From the [Material Design Lite documentation](https://material.google.com/components/sliders.html):

> The Material Design Lite (MDL) slider component is an enhanced version of the
> new HTML5 `<input type="range">` element. A slider consists of a horizontal line
> upon which sits a small, movable disc (the thumb) and, typically, text that
> clearly communicates a value that will be set when the user moves it.
>
> Sliders are a fairly new feature in user interfaces, and allow users to choose a
> value from a predetermined range by moving the thumb through the range (lower
> values to the left, higher values to the right). Their design and use is an
> important factor in the overall user experience. See the slider component's
> [Material Design specifications](https://material.google.com/components/sliders.html) page for details.
>
> The enhanced slider component may be initially or programmatically disabled.

See also the
[Material Design Specification](https://material.google.com/components/sliders.html).

Refer to [this site](http://debois.github.io/elm-mdl/#sliders)
for a live demo.

*NOTE* Currently does not work properly on [Microsoft Edge](https://github.com/google/material-design-lite/issues/1625)

#View

@docs view

# Properties

@docs Property
@docs value, min, max, disabled
@docs step 

# Events
@docs onChange

-}

import Html exposing (..)
import Html.Attributes as Html
import Material.Options as Options exposing (cs, css, when)
import Material.Options.Internal as Internal
import Material.Helpers as Helpers
import Json.Decode as Json


-- PROPERTIES


type alias Config m =
    { value : Float
    , min : Float
    , max : Float
    , step : Float
    , input : List (Options.Style m)
    , container : List (Options.Style m)
    }


defaultConfig : Config m
defaultConfig =
    { value = 50
    , min = 0
    , max = 100
    , step = 1
    , input = []
    , container = []
    }


{-| Properties for Slider options.
-}
type alias Property m =
    Options.Property (Config m) m


{-| Sets current value
-}
value : Float -> Property m
value =
    (\v options -> { options | value = v })
        >> Internal.option


{-| Sets the step. Defaults to 0
-}
min : Float -> Property m
min =
    (\v options -> { options | min = v })
        >> Internal.option


{-| Sets the step. Defaults to 100
-}
max : Float -> Property m
max =
    (\v options -> { options | max = v })
        >> Internal.option


{-| Sets the step. Defaults to 1
-}
step : Float -> Property m
step =
    (\v options -> { options | step = v })
        >> Internal.option

 
{-| Disables the slider
-}
disabled : Property m
disabled =
    Internal.attribute <| Html.disabled True


floatVal : Json.Decoder Float
floatVal =
    (Json.at [ "target", "valueAsNumber" ] Json.float)


{-| onChange listener for slider values

This convenience handler listens for both "change" and "input" events.
-}
onChange : (Float -> m) -> Property m
onChange f =
    Options.many
        [ Options.on "change" (Json.map f floatVal)
        , Options.on "input" (Json.map f floatVal)
        ]


{-| A slider consists of a horizontal line upon which sits a small, movable
disc (the thumb) and, typically, text that clearly communicates a value that
will be set when the user moves it. Example use:

    import Material.Slider as Slider

    slider : Model -> Html Msg
    slider model =
      p [ style [ ("width", "300px") ] ]
        [ Slider.view
            [ Slider.onChange SliderMsg
            , Slider.value model.value
            ]
        ]
-}
view : List (Property m) -> Html m
view options =
    let
        summary =
            Internal.collect defaultConfig options

        config =
            summary.config

        fraction =
            (config.value - config.min) / (config.max - config.min)

        lower =
            (toString fraction) ++ " 1 0%"

        upper =
            (toString (1 - fraction)) ++ " 1 0%"

        -- NOTE: does not work with IE yet. needs mdl-slider__ie-container
        -- and some additional logic
        background =
            Options.styled Html.div
                [ cs "mdl-slider__background-flex" ]
                [ Options.styled Html.div
                    [ cs "mdl-slider__background-lower"
                    , css "flex" lower
                    ]
                    []
                , Options.styled Html.div
                    [ cs "mdl-slider__background-upper"
                    , css "flex" upper
                    ]
                    []
                ]
    in
        Internal.applyContainer summary
            Html.div
            [ cs "mdl-slider__container" ]
            [ Internal.applyInput summary
                Html.input
                [ cs "mdl-slider"
                , cs "mdl-js-slider"
                , cs "is-upgraded"
                , cs "is-lowest-value" |> when (fraction == 0)
                  -- FIX for Firefox problem where you had to click on the 2px tall slider to initiate drag
                , css "padding" "8px 0"
                  -- FIX for MS Edge problem with input "range" height not depending on thumb height and clipping it
                , css "min-height" "20px"
                , Internal.attribute <| Html.type_ "range"
                , Internal.attribute <| Html.max (toString config.max)
                , Internal.attribute <| Html.min (toString config.min)
                , Internal.attribute <| Html.step (toString config.step)
                , Internal.attribute <| Html.value (toString config.value)
                , Internal.attribute <| Helpers.blurOn "mouseup"
                ]
                []
            , background
            ]
