module Material.Select.Item
    exposing
        ( Model
        , Config
        , defaultConfig
        , Property
        , onSelect
        , disabled
        , divider
        , ripple
        )

import Html exposing (Html)
import Material.Options.Internal as Options


-- MODEL


{-| Type of menu items
-}
type alias Model m =
    ( List (Property m), List (Html m) )



-- PROPERTIES


type alias Property m =
    Options.Property (Config m) m


type alias Config m =
    { onSelect : Maybe m
    , enabled : Bool
    , divider : Bool
    , ripple : Bool
    }


defaultConfig : Config m
defaultConfig =
    { onSelect = Nothing
    , enabled = True
    , divider = False
    , ripple = False
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


ripple : Property m
ripple =
    Options.option (\config -> { config | ripple = True })
