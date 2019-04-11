module Material.Menu
    exposing
        ( Property
        , icon
        , bottomLeft
        , bottomRight
        , topLeft
        , topRight
        , index

        , Item
        , item

        , render
        , react

        , subscriptions
        , subs

        , Model
        , defaultModel
        , Msg
        , update
        , view
        )

{-| From the [Material Design Lite documentation](http://www.getmdl.io/components/#menus-section):

> The Material Design Lite (MDL) dropdown component is a user interface element
> that allows users to select one of a number of options. The selection
> typically results in an action initiation, a setting change, or other
> observable effect. Menu options are always presented in sets of two or
> more, and options may be programmatically enabled or disabled as required.
> The dropdown appears when the user is asked to choose among a series of
> options, and is usually dismissed after the choice is made.

> Menus are an established but non-standardized feature in user interfaces,
> and allow users to make choices that direct the activity, progress, or
> characteristics of software. Their design and use is an important factor in
> the overall user experience. See the dropdown component's Material Design
> specifications page for details.

See also the
[Material Design Specification]([https://www.google.com/design/spec/components/menus.html).

Refer to [this site](https://debois.github.io/elm-mdl/#menus)
for a live demo.

# Subscriptions

The Menu component requires subscriptions to arbitrary mouse clicks to be set
up. Example initialisation of containing app:

    import Material.Menu as Menu
    import Material

    type Model =
      { …
      , mdl : Material.Model
      }

    type Msg =
      …
      | Mdl (Material.Msg Msg)

    App.program
      { init = init
      , view = view
      , subscriptions = Menu.subs Mdl model
      , update = update
      }

# Import

Along with this module you will want to to import Material.Dropdown.Item.

# Render
@docs render, subs

# Item
@docs Item, item

# Options
@docs Property

## Alignment
@docs bottomLeft, bottomRight, topLeft, topRight

## Appearance
@docs icon, index

# Elm architecture
@docs Model, defaultModel, Msg, update, view, subscriptions

# Internal use
@docs react

-}

import DOM exposing (Rectangle)
import Html.Attributes as Html
import Html.Events as Html exposing (defaultOptions)
import Html exposing (..)
import Json.Decode as Json exposing (Decoder)
import Json.Encode exposing (string)
import Material.Component as Component exposing (Indexed)
import Material.Dropdown as Dropdown
import Material.Dropdown.Item as Item
import Material.Helpers as Helpers exposing (pure, map1st)
import Material.Icon as Icon
import Material.Internal.Dropdown as Dropdown
import Material.Internal.Geometry as Geometry exposing (Geometry)
import Material.Internal.Menu exposing (ItemInfo, Msg(..))
import Material.Internal.Options as Internal
import Material.Msg exposing (Index)
import Material.Options as Options exposing (Style, cs, css, styled, styled_, when)
import Material.Ripple as Ripple
import Mouse
import String


-- CONSTANTS


constant :
    { transitionDurationSeconds : Float
    , transitionDurationFraction : Float
    , closeTimeout : Float
    }
constant =
    { transitionDurationSeconds = 0.3
    , transitionDurationFraction = 0.8
    , closeTimeout = 150
    }


transitionDuration : Float
transitionDuration =
    constant.transitionDurationSeconds
        * constant.transitionDurationFraction


{-| Component subscriptions.
-}

-- TODO: Right now I need alignment to figure this out, which is only available
-- in Config m / view.
subscriptions : Model -> Sub (Msg m)
subscriptions model =
    if model.dropdown.open then
        -- Mouse.clicks (Dropdown.Click alignment???)
        -- TODO ^^^^^
        Mouse.clicks (Dropdown.Click Dropdown.TopLeft >> DropdownMsg)
    else
        Sub.none



-- MODEL


{-| Component model
-}
type alias Model =
    { dropdown : Dropdown.Model
    , ignoreClick : Maybe Int
    }


{-| TODO
-}
type alias Item m =
  Item.Model m


{-| TODO
-}
item : List (Item.Property m) -> List (Html m) -> Item m
item =
    Item.item


type alias ItemConfig m=
    { enabled : Bool
    , divider : Bool
    , onSelect : Maybe m
    }


defaultItemConfig : ItemConfig m
defaultItemConfig =
    { enabled = True
    , divider = False
    , onSelect = Nothing
    }


{-| Default component model
-}
defaultModel : Model
defaultModel =
    { dropdown = Dropdown.defaultModel
    , ignoreClick = Nothing
    }



-- ACTION, UPDATE


{-| Component action.
-}
type alias Msg m
    = Material.Internal.Menu.Msg m


type alias ItemIndex
    = Material.Internal.Menu.ItemIndex


type alias KeyCode
    = Material.Internal.Menu.KeyCode


{-| Component update.
-}
update : (Msg msg -> msg) -> Msg msg -> Model -> ( Model, Cmd msg )
update fwd msg model =
    case msg of

        Click a v ->
          case model.ignoreClick of
              Just 2 ->
                { model | ignoreClick = Just 1 } ! []
              Just _ ->
                { model | ignoreClick = Nothing } ! []
              Nothing ->
                update fwd (DropdownMsg (Dropdown.Click a v)) model

        Open g ->
          case model.ignoreClick of
              Just 2 ->
                { model | ignoreClick = Just 1 } ! []
              Just _ ->
                { model | ignoreClick = Nothing } ! []
              Nothing ->
                update fwd (DropdownMsg (Dropdown.Open g)) model

        Close ->
          case model.ignoreClick of
              Just 2 ->
                { model | ignoreClick = Just 1 } ! []
              Just _ ->
                { model | ignoreClick = Nothing } ! []
              Nothing ->
                update fwd (DropdownMsg Dropdown.Close) model

        Key defaultIndex itemInfos keyCode g ->
          update fwd (DropdownMsg (Dropdown.Key defaultIndex itemInfos keyCode g)) model
          |> -- Prevent next click triggered by quirks mode + subscriptions
             -- when opening with SPACE..
             ( if keyCode == 32 then
                       if not model.dropdown.open then
                           ( \( model, cmds ) -> { model | ignoreClick = Just 2 } ! [ cmds ] )
                         else
                           ( \( model, cmds ) -> { model | ignoreClick = Just 1 } ! [ cmds ] )
                   else
                       identity
             )

        DropdownMsg msg_ ->
            let
              ( dropdown, cmds ) =
                  Dropdown.update (DropdownMsg >> fwd) msg_ model.dropdown
            in
              { model | dropdown = dropdown } ! [ cmds ]


-- PROPERTIES


type alias Config m =
    { dropdown : List (Dropdown.Property m)
    , icon : String
    }


defaultConfig : Config m
defaultConfig =
    { dropdown = []
    , icon = "more_vert"
    }


{-| Type of Menu options
-}
type alias Property m =
    Options.Property (Config m) m


dropdownOption : Dropdown.Property m -> Property m
dropdownOption option =
    Internal.option (\config -> { config | dropdown = option :: config.dropdown })


{-| Menu extends from the bottom-left of the icon.
(Suitable for the dropdown-icon sitting in a top-left corner)
-}
bottomLeft : Property m
bottomLeft =
    dropdownOption Dropdown.bottomLeft


{-| Menu extends from the bottom-right of the icon.
(Suitable for the dropdown-icon sitting in a top-right corner)
-}
bottomRight : Property m
bottomRight =
    dropdownOption Dropdown.bottomRight


{-| Menu extends from the top-left of the icon.
(Suitable for the dropdown-icon sitting in a lower-left corner)
-}
topLeft : Property m
topLeft =
    dropdownOption Dropdown.topLeft


{-| Menu extends from the rop-right of the icon.
(Suitable for the dropdown-icon sitting in a lower-right corner)
-}
topRight : Property m
topRight =
    dropdownOption Dropdown.topRight


{-| Set the dropdown icon
-}
icon : String -> Property m
icon =
    Internal.option << (\name config -> { config | icon = name })


{-| Set the default value of a menu.
-}
index : Int -> Property m
index =
    dropdownOption << Dropdown.index



-- VIEW


{-| Component view.
-}
view
    : (Msg m -> m)
    -> Model
    -> List (Property m)
    -> List (Item m)
    -> Html m
view lift model properties items =
    let
        ({ config } as summary) =
            Internal.collect defaultConfig properties

        defaultIndex =
            if model.dropdown.index /= Nothing then
                model.dropdown.index
            else
                dropdownConfig.index

        dropdownSummary =
            Internal.collect Dropdown.defaultConfig config.dropdown

        dropdownConfig =
            dropdownSummary.config

        itemSummaries =
            List.map (Internal.collect Item.defaultConfig << .options) items

        itemInfos =
            itemSummaries
            |> List.map (\{ config } ->
                   { enabled = config.enabled
                   , onSelect = config.onSelect
                   }
               )

        button =
            -- TODO: trigger
            styled_ Html.button
            [ cs "mdl-button"
            , cs "mdl-js-button"
            , cs "mdl-button--icon"
            , Options.on "click"
                ( Json.map
                     (if model.dropdown.open then always Close else Open)
                     decodeGeometry
                  |> Json.map lift
                )
            , Options.on "keydown"
                  ( Json.map2
                        (Key defaultIndex itemInfos)
                        Html.keyCode
                        decodeGeometry
                    |> Json.map lift
                  )
            ]
            [ Html.attribute "onkeydown" """javascript:
                  if ((event.keyCode == 38) || (event.keyCode == 40)) {
                      event.preventDefault();
                  }
                  if (event.keyCode == 32) {
                      //return false;
                  }
              """
            ]
            [ Icon.view menu.icon
                [ cs "material-icons"
                , css "pointer-events" "none"
                ]
            ]
    in
        Internal.apply summary
            div
            (css "position" "relative" :: properties)
            []
            [ button
            , Dropdown.view (DropdownMsg >> lift) model.dropdown config.dropdown
--              [ when (dropdownConfig.index /= Nothing)
--                    (Dropdown.index (config.index |> Maybe.withDefault 0))
--              ]
              items
            ]


-- COMPONENT


type alias Store s =
    { s | menu : Indexed Model }


( get, set ) =
    Component.indexed .menu (\x y -> { y | menu = x }) defaultModel


{-| Component react function. Internal use only.
-}
react :
    (Msg m -> m)
    -> Msg m
    -> Index
    -> Store s
    -> ( Maybe (Store s), Cmd m )
react lift msg idx store =
    update lift msg (get idx store)
        |> map1st (set idx store >> Just)


{-| Component render. Below is an example, assuming boilerplate setup as
indicated in `Material`, and a user message `Select String`.

    Menu.render Mdl [idx] model.mdl
      [ Menu.topLeft, Menu.ripple ]
      [ Menu.item
        [ onSelect Select "Some item" ]
        [ text "Some item" ]
      , Menu.item
        [ onSelect "Another item", Menu.divider ]
        [ text "Another item" ]
      , Menu.item
        [ onSelect "Disabled item", Menu.disabled ]
        [ text "Disabled item" ]
      , Menu.item
        [ onSelect "Yet another item" ]
        [ text "Yet another item" ]
      ]
-}
render :
    (Material.Msg.Msg m -> m)
    -> Index
    -> Store s
    -> List (Property m)
    -> List (Item m)
    -> Html m
render =
    Component.render get view Material.Msg.MenuMsg


{-| TODO
-}
subs : (Material.Msg.Msg m -> m) -> Store s -> Sub m
subs =
    Component.subs Material.Msg.MenuMsg .menu subscriptions


-- HELPERS


decodeGeometry : Decoder Geometry
decodeGeometry =
    Json.map5 Geometry
        (DOM.target Geometry.element)
        (DOM.target (DOM.nextSibling (DOM.childNode 1 Geometry.element)))
        (DOM.target (DOM.nextSibling Geometry.element))
        (DOM.target (DOM.nextSibling (DOM.childNode 1 (DOM.childNodes DOM.offsetTop))))
        (DOM.target (DOM.nextSibling (DOM.childNode 1 (DOM.childNodes DOM.offsetHeight))))


rect : Float -> Float -> Float -> Float -> String
rect x y w h =
    [ x, y, w, h ]
        |> List.map toPx
        |> String.join " "
        |> (\coords -> "rect(" ++ coords ++ ")")


toPx : Float -> String
toPx =
    toString >> flip (++) "px"
