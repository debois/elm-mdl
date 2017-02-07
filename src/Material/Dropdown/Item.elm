module Material.Dropdown.Item
    exposing
        ( Model
        , item

        , Property
        , Config
        , defaultConfig

        , onSelect
        , disabled
        , divider
        , ripple
        , options
        , selected

        , view
        , Msg(..)
        )

{-| TODO

# Item
@docs item

# Listeners
@docs onSelect

# Property
@docs selected, disabled, divider, ripple, options

# API
@docs Property, Config, defaultConfig

# Elm architecture
@docs Model, Msg, view

@docs onSelect, disabled, divider, ripple
-}

import Dict exposing (Dict)
import Html.Attributes exposing (property, attribute, class)
import Html.Events as Html
import Html exposing (Html)
import Html exposing (li)
import Json.Encode as Json exposing (int)
import Material.Options exposing (Style, cs, css, when)
import Material.Options.Internal as Options
import Material.Ripple as Ripple


-- MODEL


{-| Type of menu items
-}
type alias Model m =
    { html : List (Html m)
    , options : List (Property m)
    }


{-| TODO
-}
type alias ItemIndex =
    Int


{-| TODO
-}
item : List (Property m) -> List (Html m) -> Model m
item options html =
  { html = html
  , options = options
  }


{-| TODO
-}
type Msg m =
      Select (Maybe m)
    | Ripple Ripple.Msg


{-| TODO
-}
type alias Store s =
    { s | ripples : Dict ItemIndex Ripple.Model
        , index : Maybe ItemIndex
        , open : Bool
    }


{-| TODO
-}
view :
    (Msg msg -> msg)
    -> Store s
    -> ItemIndex
    -> Model msg
    -> List (Property msg)
    -> ( String, Html msg )
view lift top index model defaultOptions =
    let
        options =
            model.options

        html =
            model.html

        canSelect =
            config.enabled && config.onSelect /= Nothing

        hasRipple =
            config.ripple && canSelect

        ripple =
            top.ripples
                |> Dict.get index
                |> Maybe.withDefault Ripple.model

        fwdRipple =
            Ripple >> lift

        ({ config } as summary) =
            Options.collect defaultConfig (defaultOptions ++ options)
    in
        (,) (toString index) <|
            Options.apply summary
                li
                [ cs "mdl-menu__item"
                , when config.ripple (cs "mdl-js-ripple-effect")
                , when config.divider (cs "mdl-menu__item--full-bleed-divider")
                , when config.selected (cs "mdl-menu__item--selected")
                , css "display" "flex"
                , css "align-items" "center"
                ]
                (List.filterMap identity
                    [ if canSelect then
                        Html.onClick (Select summary.config.onSelect |> lift) |> Just
                      else
                        Nothing
                    , if not summary.config.enabled then
                        attribute "disabled" "disabled" |> Just
                      else
                        Nothing
                    , property "tabIndex" (int (-1)) |> Just
                    ]
                    ++ (if hasRipple then
                            [ Ripple.downOn_ fwdRipple "mousedown"
                            , Ripple.downOn_ fwdRipple "touchstart"
                            , Ripple.upOn_ fwdRipple "mouseup"
                            , Ripple.upOn_ fwdRipple "mouseleave"
                            , Ripple.upOn_ fwdRipple "touchend"
                            , Ripple.upOn_ fwdRipple "blur"
                            ]
                        else
                            []
                       )
                )
                (if hasRipple then
                    ((++) html
                        [ Ripple.view_ [ class "mdl-menu__item-ripple-container" ] ripple
                            |> Html.map fwdRipple
                        ]
                    )
                 else
                    html
                )


-- PROPERTIES


{-| TODO
-}
type alias Property m =
    Options.Property (Config m) m


{-| TODO
-}
type alias Config m =
    { onSelect : Maybe m
    , enabled : Bool
    , divider : Bool
    , ripple : Bool
    , options : List (Style m)
    , selected : Bool
    }


{-| TODO
-}
defaultConfig : Config m
defaultConfig =
    { onSelect = Nothing
    , enabled = True
    , divider = False
    , ripple = False
    , options = []
    , selected = False
    }


{-| Handle selection of containing item
-}
onSelect : m -> Property m
onSelect msg =
    Options.option (\config -> { config | onSelect = Just msg })


{-| Mark item as disabled.
-}
disabled : Property m
disabled =
    Options.option (\config -> { config | enabled = False })


{-| Render a dividing line before the item
-}
divider : Property m
divider =
    Options.option (\config -> { config | divider = True })


{-| Menu item ripples when clicked
-}
ripple : Property m
ripple =
    Options.option (\config -> { config | ripple = True })


{-| TODO
-}
options : List (Style m)-> Property m
options v =
    Options.option (\config -> { config | options = v })


{-| TODO
-}
selected : Property m
selected =
    Options.option (\config -> { config | selected = True })
