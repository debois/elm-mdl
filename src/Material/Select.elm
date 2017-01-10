module Material.Select
    exposing
        ( Model
        , defaultModel
        , Msg
        , update
        , view
        , render
        , react

        , Property
        , ripple
        , value
        , item
        , disabled
        , label
        , error
        , floatingLabel
        , index
        , subscriptions
        , subs
        , closeAll
        )

{-| Refer to [this site](https://debois/github.io/elm-mdl/#select)
for a live demo.

# Subscriptions

The select component requires subscriptions to arbitrary mouse clicks to be set
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

The select component closes itself on pressing TAB. It does not close
automatically if focus another component by other means, ie. clicking.

If you want select components to close if another element receives focus, you
will have to listen to that element's focus event and call
`Material.Select.closeAll` from your program.

# Import

Along with this module you will probably want to import Material.Select.Item.

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
import Html.Keyed
import Json.Decode as Json exposing (Decoder)
import Json.Encode exposing (string, int)
import Material.Component as Component exposing (Indexed, Index)
import Material.Helpers as Helpers exposing (fst, snd, pure, map1st)
import Material.Icon as Icon
import Material.Options as Options exposing (Style, cs, css, styled, styled_, when)
import Material.Options.Internal as Internal
import Material.Ripple as Ripple
import Material.Select.Item as Item
import Mouse
import String


{-| Component subscriptions.
-}
subscriptions : Model -> Sub (Msg m)
subscriptions model =
    if model.open then
        Mouse.clicks Click
    else
        Sub.none


-- MODEL


{-| Component model
-}
type alias Model =
    { ripples : Dict Int Ripple.Model
    , open : Bool
    , geometry : Maybe Geometry
    , index : Maybe Int
    }


{-| Default component model
-}
defaultModel : Model
defaultModel =
    { ripples = Dict.empty
    , open = False
    , geometry = Nothing
    , index = Nothing
    }


-- ITEM


{-| Construct an menu item.
-}
item :
    List (Item.Property m)
    -> List (Html m)
    -> Item.Model m
item =
    (,)


-- Note: Other functions in Material.Select.Item, because most of them share
-- names.


-- ACTION, UPDATE


{-| Component action.
-}
type Msg m
    = Open Geometry
    | Select Int (Maybe m)
    | Close
    | Click Mouse.Position
    | Ripple Int Ripple.Msg
    | Input String
    | Key (Maybe Int) (List (Internal.Summary (Item.Config m) m)) Int


{-| Component update.
-}
update : (Msg msg -> msg) -> Msg msg -> Model -> ( Model, Cmd msg )
update fwd msg model =
    case msg of
        -- This is just here to trigger a DOM update. Ideally, we want to prevent
        -- the default action of "input", but we cannot because of the Tab key.
        Input _ ->
            model ! []

        Open geometry ->
            { model
                | open = True
                , geometry = Just geometry
                , index = Nothing
            }
                ! []

        Close ->
            { model
                | open = False
                , geometry = Nothing
                , index = Nothing
            }
                ! []

        Select idx msg ->
            -- Close the menu after some delay for the ripple effect to show.
            { model
                | index = Just idx
            }
                ! (List.filterMap identity
                    [ Helpers.delay 150 (fwd Close) |> Just
                    , msg |> Maybe.map Helpers.cmd
                    ]
                  )

        Ripple idx action ->
            let
                ( model_, cmd ) =
                    Dict.get idx model.ripples
                        |> Maybe.withDefault Ripple.model
                        |> Ripple.update action
            in
                { model | ripples = Dict.insert idx model_ model.ripples }
                    ! [ Cmd.map (Ripple idx >> fwd) cmd ]

        Click pos ->
            let
                inside { x, y } { top, left, width, height } =
                    (left <= toFloat x)
                        && (toFloat x <= left + width)
                        && (top <= toFloat y)
                        && (toFloat y <= top + height)

                g =
                    Maybe.withDefault defaultGeometry model.geometry

                container =
                    containerGeometry g.menu
            in
                if (model.open && not (inside pos container || inside pos g.input)) then
                    update fwd Close model
                else
                    model ! []

        Key defaultIndex summaries keyCode ->
            case keyCode of
                13 ->
                    -- ENTER
                    let
                        index =
                            if model.index /= Nothing then
                                model.index
                            else
                                defaultIndex
                    in
                        case index of
                            Just index_ ->
                                let
                                    cmd =
                                        List.drop index_ summaries
                                            |> List.head
                                            |> Maybe.andThen (.config >> .onSelect)
                                in
                                    update fwd (Select index_ cmd) model

                            _ ->
                                model ! []

                32 ->
                    -- SPACE
                    update fwd (Key defaultIndex summaries 13) model

                9 ->
                    -- TAB
                    update fwd Close model

                40 ->
                    -- DOWN_ARROW
                    let
                        index =
                            Maybe.withDefault -1 <|
                                if model.index /= Nothing then
                                    model.index
                                else
                                    defaultIndex

                        items =
                            List.indexedMap (,) summaries

                        numItems =
                            List.length summaries
                    in
                        (items ++ items)
                            |> List.drop (1 + index)
                            |> List.filter (snd >> .config >> .enabled)
                            |> List.head
                            |> Maybe.map
                                (fst
                                    >> \index_ ->
                                        { model
                                            | index = Just index_
                                        }
                                )
                            |> Maybe.withDefault model
                            |> flip (!) []

                38 ->
                    -- UP_ARROW
                    let
                        index =
                            Maybe.withDefault 0 <|
                                if model.index /= Nothing then
                                    model.index
                                else
                                    defaultIndex

                        items =
                            List.indexedMap (,) summaries

                        numItems =
                            List.length summaries
                    in
                        (items ++ items)
                            |> List.reverse
                            |> List.drop (numItems - index)
                            |> List.filter (snd >> .config >> .enabled)
                            |> List.head
                            |> Maybe.map
                                (fst
                                    >> \index_ ->
                                        { model
                                            | index = Just index_
                                        }
                                )
                            |> Maybe.withDefault model
                            |> pure

                _ ->
                    model ! []



-- PROPERTIES


type alias Config m =
    { ripple : Bool
    , labelText : Maybe String
    , labelFloat : Bool
    , error : Maybe String
    , disabled : Bool
    , autofocus : Bool
    , inner : List (Options.Style m)
    , listeners : List (Maybe Int -> Html.Attribute m)
    , index : Maybe Int
    , value : String
    }


defaultConfig : Config m
defaultConfig =
    { ripple = False
    , labelText = Nothing
    , labelFloat = False
    , error = Nothing
    , disabled = False
    , autofocus = False
    , inner = []
    , listeners = []
    , index = Nothing
    , value = ""
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


{-| Label of select animates away from the input area on input
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


{-| Specifies tha the select should automatically get focus when the page loads
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
    -> List (Item.Model m)
    -> Html m
view lift model properties items =
    let
        ({ config } as summary) =
            Internal.collect defaultConfig properties

        g =
            model.geometry
                |> Maybe.withDefault defaultGeometry

        container =
            containerGeometry g.menu

        menu =
            menuGeometry g.menu

        numItems =
            List.length items

        hasRipple =
            config.ripple

        fwdRipple =
            Ripple -1 >> lift

        ripple =
            model.ripples
                |> Dict.get -1
                |> Maybe.withDefault Ripple.model

        defaultIndex =
            if model.index /= Nothing then
                model.index
            else
                config.index

        itemSummaries =
            List.map (Internal.collect Item.defaultConfig << fst) items
    in
        Internal.apply summary
            div
            ([ [ cs "mdl-select"
               , cs "mdl-select--floating-label" |> when config.labelFloat
               , cs "mdl-js-ripple-effect" |> when config.ripple
               , cs "is-invalid" |> when (config.error /= Nothing)
               , cs "is-dirty" |> when (config.value /= "")
               , cs "is-focused" |> when model.open
               , cs "is-disabled" |> when config.disabled
               ]
             , properties
             ]
                |> List.concat
            )
            []
            [ styled_ Html.div
                [ cs "mdl-select__trigger"
                ]
                [ Ripple.downOn_ fwdRipple "mousedown"
                , Ripple.downOn_ fwdRipple "touchstart"
                , Ripple.upOn_ fwdRipple "mouseup"
                , Ripple.upOn_ fwdRipple "mouseleave"
                , Ripple.upOn_ fwdRipple "touchend"
                , Ripple.upOn_ fwdRipple "blur"
                , attribute "onclick" "this.nextSibling.focus()"
                ]
                [ Ripple.view_ [] ripple
                    |> Html.map fwdRipple
                ]
            , styled_ Html.input
                [ cs "mdl-select__input"
                , css "outline" "none"
                    , Internal.attribute (Html.on "keydown" (Json.map (Key defaultIndex itemSummaries >> lift) Html.keyCode))
                    |>
                        when model.open
                , Internal.attribute (Html.on "focus" (Json.map (Open >> lift) decodeGeometryAsInput))
                    |> when (not model.open)
                , Internal.attribute (Html.on "input" (Json.map (Input >> lift) Html.targetValue))
                , Options.many config.inner
                ]
                (List.concat
                    [ [ Attributes.disabled config.disabled
                      , Attributes.autofocus config.autofocus
                      , type_ "text"
                      , Attributes.value config.value
                      , attribute "onkeydown" "if ((event.keyCode == 13) || (event.keyCode == 32)) {this.blur();}"
                      ]
                    , List.map ((|>) defaultIndex) config.listeners
                    ]
                )
                []
            , styled div
                [ cs "mdl-menu__container"
                , cs "is-upgraded"
                , cs "is-visible" |> when model.open
                , css "width" (container.width |> toPx) |> when (model.geometry /= Nothing)
                , css "height" (container.height |> toPx) |> when (model.geometry /= Nothing)
                ]
                [ styled div
                    [ cs "mdl-menu__outline"
                    , css "width" (container.width |> toPx) |> when (model.geometry /= Nothing)
                    , css "height" (container.height |> toPx) |> when (model.geometry /= Nothing)
                    ]
                    []
                , styled_ Html.Keyed.ul
                    [ cs "mdl-menu"
                    , cs "mdl-js-menu"
                    , clip model container |> when (model.geometry /= Nothing)
                    , css "width" (menu.width |> toPx) |> when (model.geometry /= Nothing)
                    , css "height" (menu.height |> toPx) |> when (model.geometry /= Nothing)
                    , css "overflow-y" "auto"
                    , css "overflow-x" "hidden"
                    ]
                    [ property "tabIndex" (int -1)
                    ]
                    (List.indexedMap (view1 lift model config numItems) items)
                ]
            , Html.label
                [ class "mdl-select__label" ]
                (case config.labelText of
                    Just str ->
                        [ text str ]

                    Nothing ->
                        []
                )
            , config.error
                |> Maybe.map (\e -> span [ class "mdl-select__error" ] [ text e ])
                |> Maybe.withDefault (div [] [])
            , Icon.view "expand_more" []
                |> Html.map lift
            ]


view1 :
    (Msg m -> m)
    -> Model
    -> Config m
    -> Int
    -> Int
    -> Item.Model m
    -> ( String, Html m )
view1 lift top topConfig numItems index ( options, html ) =
    let
        canSelect =
            config.enabled && config.onSelect /= Nothing

        hasRipple =
            config.ripple && canSelect

        ripple =
            top.ripples
                |> Dict.get index
                |> Maybe.withDefault Ripple.model

        fwdRipple =
            Ripple index >> lift

        defaultIndex =
            if top.index /= Nothing then
                top.index
            else
                topConfig.index

        ({ config } as summary) =
            Internal.collect Item.defaultConfig options
    in
        (,) (toString index) <|
            Internal.apply summary
                li
                [ cs "mdl-menu__item"
                , cs "mdl-js-ripple-effect" |> when config.ripple
                , cs "mdl-menu__item--full-bleed-divider" |> when summary.config.divider
                , cs "mdl-menu__item--selected" |> when (defaultIndex == Just index)
                , delay index numItems |> when top.open
                ]
                (List.filterMap identity
                    [ if canSelect then
                        Html.onClick
                            (Select index summary.config.onSelect |> lift)
                            |> Just
                      else
                        Nothing
                    , if not summary.config.enabled then
                        attribute "disabled" "disabled" |> Just
                      else
                        Nothing
                    , property "tabIndex" (int -1) |> Just
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


delay : Int -> Int -> Options.Property (Item.Config m) m
delay index numItems =
    css "transition-delay" <| toString (0.24 * toFloat index / toFloat numItems) ++ "s"


menuGeometry : Rectangle -> Rectangle
menuGeometry menu =
    { top = menu.top
    , left = menu.left
    , width = menu.width
    , height = menu.height
    }


containerGeometry : Rectangle -> Rectangle
containerGeometry menu =
    { top = menu.top
    , left = menu.left
    , width = menu.width
    , height = menu.height
    }


clip : Model -> Rectangle -> Options.Property c m
clip model g =
    css "clip" <|
        if model.open then
            rect 0 g.width g.height 0
        else
            rect 0 g.width 0 g.width


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
    import Material.Select.Item as Item

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
    (Component.Msg button textfield menu layout toggles tooltip tabs (Msg m) dispatch -> m)
    -> Component.Index
    -> Store s
    -> List (Property m)
    -> List (Item.Model m)
    -> Html m
render =
    Component.render get view Component.SelectMsg


{-| TODO
-}
subs :
    (Component.Msg button textfield menu layout toggles tooltips tabs (Msg m) dispatch -> msg)
    -> Store s
    -> Sub msg
subs =
    Component.subs Component.SelectMsg .select subscriptions


{-| Closes all select components within a Store.

Example:

    Material.update Mdl (Select.closeAll Mdl model.mdl) model
-}
closeAll
    : (
    Component.Msg
        button
        textfield
        menu
        layout
        toggles
        tooltip
        tabs
        (Msg m)
        dispatch
    -> m
    )
    -> Store s
    -> Component.Msg
           button
           textfield
           menu
           layout
           toggles
           tooltip
           tabs
           select
           (List m)
closeAll lift store =
    store.select
        |> Dict.map (\idx model ->
               if model.open then
                       Component.SelectMsg idx Close |> Just << lift
                   else
                       Nothing
           )
        |> Dict.values
        |> List.filterMap identity
        |> Component.Dispatch


-- GEOMETRY


{-| An Geometry stores relevant information from DOM during Open and Close
events. (This computes more than it needs to.)
-}
type alias Geometry =
    { input : Rectangle
    , menu : Rectangle
    , items : List Rectangle
    }


defaultGeometry : Geometry
defaultGeometry =
    { input = defaultRectangle
    , menu = defaultRectangle
    , items = []
    }


defaultRectangle : Rectangle
defaultRectangle =
    { top = 0, left = 0, width = 0, height = 0 }


{-| Decode Geometry from the triggers's reference
-}
decodeGeometryAsTrigger : Decoder Geometry
decodeGeometryAsTrigger =
    Json.map3 Geometry
        (DOM.target (DOM.nextSibling DOM.boundingClientRect))
        (DOM.target (DOM.nextSibling (DOM.nextSibling (DOM.childNode 1 DOM.boundingClientRect))))
        (Json.succeed [])


{-| Decode Geometry from the input's reference
-}
decodeGeometryAsInput : Decoder Geometry
decodeGeometryAsInput =
    Json.map3 Geometry
        (DOM.target DOM.boundingClientRect)
        (DOM.target (DOM.nextSibling (DOM.childNode 1 DOM.boundingClientRect)))
        (Json.succeed [])



-- HELPERS


rect : number -> number -> number -> number -> String
rect x y w h =
    [ x, y, w, h ]
        |> List.map toPx
        |> String.join " "
        |> (\coords -> "rect(" ++ coords ++ ")")


toPx : number -> String
toPx =
    toString >> flip (++) "px"


onFocus : m -> Property m
onFocus f =
    on "focus" (Json.succeed << always f)


onBlur : (Maybe Int -> m) -> Property m
onBlur f =
    on "focus" (Json.succeed << f)


on : String -> (Maybe Int -> Decoder m) -> Property m
on event decoder =
    Internal.option
        (\config ->
            { config | listeners = config.listeners ++ [ Html.on event << decoder ] }
        )
