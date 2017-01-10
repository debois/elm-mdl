module Material.Select
    exposing
        ( Model
        , defaultModel
        , Msg
        , update
        , view
        , render
        , react

        , item

        , Property
        , ripple
        , value
        , disabled
        , label
        , error
        , floatingLabel
        , index
        , subscriptions
        , subs
        )

{-| Refer to [this site](https://debois/github.io/elm-mdl/#select)
for a live demo.

# Subscriptions

The Select component requires subscriptions to arbitrary mouse clicks to be set
up. Example initialisation of containing app:

    import Material.Select as Select
    import Material

    type Model =
        { mdl : Material.Model -- Boilerplate
        }
    type Msg =
        …
        | Mdl Material.Msg -- Boilerplate
    …

    App.program
        { init = init
        , view = view
        , subscriptions = Select.subs Mdl model
        , update = update
        }


# Behavior

The Select component closes itself on pressing TAB. It does not close
automatically if the user focuses another component by other means, ie.
clicking.

If you want Select components to close if another element receives focus, you
will have to listen to that element's focus event and call
`Material.Select.closeAll` from your program.

# Import

Along with this module you will probably want to import Material.Shared.Item.

# Render
@docs render, subs

# Options
@docs Property

# Items
@docs item

# Appearance
@docs value, label, floatingLabel, error, disabled, index

# Appearance
@docs ripple
-}

import Dict exposing (Dict)
import DOM exposing (Rectangle)
import Html.Attributes as Attributes exposing (class, type_, attribute, property)
import Html.Events as Html exposing (defaultOptions, targetValue)
import Html exposing (..)
import Json.Decode as Json exposing (Decoder)
import Material.Component as Component exposing (Indexed, Index)
import Material.Helpers as Helpers exposing (pure, map1st)
import Material.Icon as Icon
import Material.Options as Options exposing (Style, cs, css, styled, styled_, when)
import Material.Options.Internal as Internal
import Material.Ripple as Ripple
import Material.Dropdown.Item as Item
import Material.Dropdown.Geometry as Geometry exposing (Geometry)
import Material.Textfield as Textfield
import Mouse
import String
import Material.Dropdown as Dropdown exposing (Msg)
import DOM


{-| Component subscriptions.
-}
subscriptions : Model -> Sub (Msg m)
subscriptions model =
    if model.dropdown.open then
        Mouse.clicks (Dropdown.Click Dropdown.BottomRight >> MenuMsg)
    else
        Sub.none


-- MODEL


{-| Component model
-}
type alias Model =
    { dropdown : Dropdown.Model
    , textfield : Textfield.Model
    , openOnFocus : Bool
    }


type alias Item m =
    Item.Model m


{-| Default component model
-}
defaultModel : Model
defaultModel =
    { dropdown = Dropdown.defaultModel
    , textfield = Textfield.defaultModel
    , openOnFocus = False
    }


-- ITEM


{-| Construct a dropdown item.
-}
item : List (Item.Property m) -> List (Html m) -> Item m
item =
    Item.item


-- ACTION, UPDATE


{-| Component action.
-}
type Msg m
    = Click
    | Focus Geometry
    | Blur

    | Open Geometry

    | Input String

    | MenuMsg (Dropdown.Msg m)
    | TextfieldMsg Textfield.Msg


{-| Component update.
-}
update : (Msg msg -> msg) -> Msg msg -> Model -> ( Model, Cmd msg )
update fwd msg model =
    case msg of

        MenuMsg msg_ ->
            let
              (dropdown, cmds) =
                  Dropdown.update (MenuMsg >> fwd) msg_ model.dropdown
            in
              { model | dropdown = dropdown } ! [ cmds ]

        TextfieldMsg msg_ ->
            let
              ( textfield, cmd ) =
                  Textfield.update () msg_ model.textfield
            in
              { model | textfield =
                            Maybe.withDefault model.textfield textfield
              } ! [ cmd ]

        Click ->
          { model | openOnFocus = True } ! []

        Open g ->
            let
              msg_ =
                Dropdown.Open g

              (dropdown, cmds) =
                  Dropdown.update (MenuMsg >> fwd) msg_ model.dropdown
            in
              { model | dropdown = dropdown } ! [ cmds ]

        Focus g ->
            if model.openOnFocus then
                    let
                      msg_ =
                        Dropdown.Open g

                      (dropdown, cmds) =
                          Dropdown.update (MenuMsg >> fwd) msg_ model.dropdown
                    in
                      { model | dropdown = dropdown, openOnFocus = False } ! [ cmds ]
                else
                    model ! []

        Blur ->
            { model | openOnFocus = False } ! []

        -- This is just here to trigger a DOM update. Ideally, we want to
        -- prevent the default action of "input", but we cannot because of the
        -- Tab key. (This is only useful if the dropdown's toggle is an input.)
        Input _ ->
            model ! []


-- PROPERTIES


type alias Config m =
    {

      input : List (Options.Style m)

      -- Basically Textfield:
    , labelText : Maybe String
    , labelFloat : Bool
    , error : Maybe String
    , disabled : Bool
    , autofocus : Bool
    , inner : List (Options.Style m)
    , value : String

      -- Shared.Model:
    , ripple : Bool
    , index : Maybe Int
    , listeners : List (Maybe Int -> Html.Attribute m)
    }


defaultConfig : Config m
defaultConfig =
    { input = []

    , labelText = Nothing
    , labelFloat = False
    , error = Nothing
    , disabled = False
    , autofocus = False
    , inner = []
    , value = ""

    , ripple = False
    , index = Nothing
    , listeners = []
    }


{-| Type of Select options
-}
type alias Property m =
    Options.Property (Config m) m


{-| Select highlights the `n`-th item (0-based).
-}
index : Int -> Property m
index v =
    Internal.option (\config -> { config | index = Just v })


{-| Select shows `s` as the selected value.
-}
value : String -> Property m
value v =
    Internal.option (\config -> { config | value = v })


{-| Select itself ripples when clicked
-}
ripple : Property m
ripple =
    Internal.option (\config -> { config | ripple = True })


{-| Label of the Select
-}
label : String -> Property m
label str =
    Internal.option (\config -> { config | labelText = Just str })


{-| Label of Select animates away from the input area on input
-}
floatingLabel : Property m
floatingLabel =
    Internal.option (\config -> { config | labelFloat = True })


{-| Error message
-}
error : String -> Property m
error str =
    Internal.option (\config -> { config | error = Just str })


{-| Disable the Select input
-}
disabled : Property m
disabled =
    Internal.option (\config -> { config | disabled = True })


{-| Specifies tha the Select should automatically get focus when the page loads
-}
autofocus : Property m
autofocus =
    Internal.option (\config -> { config | autofocus = True })


-- VIEW


{-| Component view.
-}
view :
    (Msg m -> m)
    -> Model
    -> List (Property m)
    -> List (Item m)
    -> Html m
view lift model properties items =
    let
        ({ config } as summary) =
            Internal.collect defaultConfig properties

        fwdRipple =
            Item.Ripple >> Dropdown.ItemMsg -1 >> MenuMsg >> lift

        ripple =
            model.dropdown.ripples
                |> Dict.get -1
                |> Maybe.withDefault Ripple.model

        defaultIndex =
            Dropdown.defaultIndex model.dropdown config.index

        itemSummaries =
            List.map (Internal.collect Item.defaultConfig << .options) items

        button =
            [ Icon.view "expand_more" [] |> Html.map lift
            , Textfield.view (TextfieldMsg >> lift) model.textfield
                  ( -- Internal.input
                    [ ( Options.on "keydown"
                          ( Json.map2
                                (Dropdown.Key defaultIndex itemSummaries)
                                Html.keyCode
                                decodeAsInput
                            |> Json.map (MenuMsg >> lift)
                          )
                      )
                    , Options.on "blur" (Json.succeed (Blur |> lift))
                    , Options.on "input" (Json.map (Input >> lift) Html.targetValue)

                    , when config.ripple
                        (Options.on "focus" (Json.map (Focus >> lift) decodeAsInput))
                    , when (not config.ripple)
                        (Options.on "click" (Json.map (Open >> lift) decodeAsInput))

                    , when config.labelFloat Textfield.floatingLabel
                    , when (config.labelText /= Nothing)
                        (Textfield.label (Maybe.withDefault "" config.labelText))
                    , when (config.error /= Nothing)
                        (Textfield.error (Maybe.withDefault "" config.error))
                    , when (config.autofocus) Textfield.autofocus
                    , when (config.disabled) Textfield.disabled
                    , Textfield.value config.value

                    -- TODO:
                    -- , Options.many config.inner

                    , List.map ((|>) defaultIndex) config.listeners
                      |> List.map Options.attribute
                      |> Options.many
                    ]
                  )
                  []
            , styled_ Html.div
                [ cs "mdl-select__trigger"
                , css "display" (if config.ripple then "block" else "none")
                , Options.on "click" (Json.succeed (Click |> lift))
                ]
                [ Ripple.downOn_ fwdRipple "mousedown"
                , Ripple.downOn_ fwdRipple "touchstart"
                , Ripple.upOn_ fwdRipple "mouseup"
                , Ripple.upOn_ fwdRipple "mouseleave"
                , Ripple.upOn_ fwdRipple "touchend"
                , Ripple.upOn_ fwdRipple "blur"
                , -- Note: Click on trigger should open the dropdown.
                  attribute "onclick" "this.previousSibling.firstChild.focus()"
                ]
                [ Ripple.view_ [] ripple
                    |> Html.map fwdRipple
                ]
          ]

        dropdown =
          Dropdown.view (MenuMsg >> lift) model.dropdown
          [ when (config.index /= Nothing)
                (Dropdown.index (config.index |> Maybe.withDefault 0))
          , Dropdown.bottomRight
          ]
          items
    in
        Internal.apply summary div
            ( cs "mdl-select"
              :: when (model.dropdown.open) (cs "mdl-js-ripple-effect")
              :: properties
            )
            []
            ( button ++ [ dropdown ]
            )


-- COMPONENT


type alias Store s =
    { s | select : Indexed Model }


( get, set ) =
    Component.indexed .select (\x y -> { y | select = x }) defaultModel


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

    import Material.Select as Select
    import Material.Shared.Item as Item

    Select.render Mdl [0] model.mdl
    [ Select.label "Dinosaurs"
    , Select.floatingLabel
    , Select.ripple
    , Select.value model.value
    ]
    ( [ "allosaurus"
      , "brontosaurus"
      , "carcharodontosaurus"
      , "diplodocus"
      ]
      |> List.map (\string ->
           Select.item
           [ Item.onSelect (Select string)
           , Item.ripple
           ]
           [ text string
           ]
         )
    )

-}
render :
    (Component.Msg button textfield dropdown layout toggles tooltip tabs (Msg m) dispatch -> m)
    -> Component.Index
    -> Store s
    -> List (Property m)
    -> List (Item m)
    -> Html m
render =
    Component.render get view Component.SelectMsg


{-| TODO
-}
subs :
    (Component.Msg button textfield dropdown layout toggles tooltips tabs (Msg m) dispatch -> msg)
    -> Store s
    -> Sub msg
subs =
    Component.subs Component.SelectMsg .select subscriptions


-- DECODER


decodeAsInput : Decoder Geometry
decodeAsInput =
  Json.map5 Geometry
      (DOM.target Geometry.element)
      (DOM.target (DOM.parentElement ((DOM.nextSibling (DOM.nextSibling (DOM.childNode 1 Geometry.element))))))
      (DOM.target (DOM.parentElement  (DOM.nextSibling (DOM.nextSibling Geometry.element))))
      (DOM.target (DOM.parentElement ((DOM.nextSibling (DOM.nextSibling (DOM.childNode 1 (DOM.childNodes DOM.offsetTop)))))))
      (DOM.target (DOM.parentElement ((DOM.nextSibling (DOM.nextSibling (DOM.childNode 1 (DOM.childNodes DOM.offsetHeight)))))))


-- TODO:
--decodeAsTrigger =
--  Json.map5 Geometry
--      (Json.succeed Geometry.defaultElement)
--      (Json.succeed Geometry.defaultElement)
--      (Json.succeed Geometry.defaultElement)
--      (Json.succeed [])
--      (Json.succeed [])
----    (DOM.target (DOM.nextSibling Geometry.element))
----    (DOM.target (DOM.nextSibling (DOM.nextSibling (DOM.childNode 1 Geometry.element))))
----    (DOM.target (DOM.nextSibling (DOM.nextSibling Geometry.element)))
----    (DOM.target (DOM.nextSibling (DOM.nextSibling (DOM.childNode 1 (DOM.childNodes DOM.offsetTop)))))
----    (DOM.target (DOM.nextSibling (DOM.nextSibling ((DOM.childNode 1 (DOM.childNodes DOM.offsetHeight))))))


rect : number -> number -> number -> number -> String
rect x y w h =
    [ x, y, w, h ]
        |> List.map toPx
        |> String.join " "
        |> (\coords -> "rect(" ++ coords ++ ")")


toPx : number -> String
toPx =
    toString >> flip (++) "px"
