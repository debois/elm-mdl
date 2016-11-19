module Material.Toggles
    exposing
        ( Model
        , defaultModel
        , Msg
        , update
        , viewSwitch
        , viewCheckbox
        , viewRadio
        , switch
        , checkbox
        , radio
        , react
        , ripple
        , disabled
        , value
        , group
        )

{-| From the [Material Design Lite documentation](http://www.getmdl.io/index.html#toggles-section/checkbox):

> The Material Design Lite (MDL) checkbox component is an enhanced version of the
> standard HTML `<input type="checkbox">` element. A checkbox consists of a small
> square and, typically, text that clearly communicates a binary condition that
> will be set or unset when the user clicks or touches it. Checkboxes typically,
> but not necessarily, appear in groups, and can be selected and deselected
> individually. The MDL checkbox component allows you to add display and click
>     effects.
>
> Checkboxes are a common feature of most user interfaces, regardless of a site's
> content or function. Their design and use is therefore an important factor in
> the overall user experience. [...]
>
> The enhanced checkbox component has a more vivid visual look than a standard
> checkbox, and may be initially or programmatically disabled.

See also the
[Material Design Specification](https://www.google.com/design/spec/components/selection-controls.html#).

Refer to [this site](http://debois.github.io/elm-mdl/#toggles)
for a live demo.

# Render
@docs checkbox, switch, radio

# Options
@docs ripple, disabled, value, group

# Elm architecture
@docs Model, defaultModel, Msg, update
@docs viewSwitch, viewCheckbox, viewRadio

# Internal use
@docs react

-}

import Html exposing (..)
import Html.Attributes exposing (type_, class, disabled, checked)
import Material.Component as Component exposing (Indexed, Index)
import Material.Options as Options exposing (Style, cs, styled, many, when, maybe)
import Material.Helpers exposing (map1st, map2nd, blurOn, filter, noAttr)
import Material.Ripple as Ripple
import Material.Options.Internal as Internal

-- MODEL


{-| Component model.
-}
type alias Model =
    { ripple : Ripple.Model
    , isFocused : Bool
    }


{-| Default component model.
-}
defaultModel : Model
defaultModel =
    { ripple = Ripple.model
    , isFocused = False
    }



-- ACTION, UPDATE


{-| Component action.
-}
type Msg
    = Ripple Ripple.Msg
    | SetFocus Bool


{-| Component update.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Ripple rip ->
            Ripple.update rip model.ripple
                |> map1st (\r -> { model | ripple = r })
                |> map2nd (Cmd.map Ripple)

        SetFocus focus ->
            ( { model | isFocused = focus }, Cmd.none )


-- OPTIONS


type alias Config m =
    { value : Bool
    , ripple : Bool
    , input : List (Options.Style m)
    , container : List (Options.Style m)
    }


defaultConfig : Config m
defaultConfig =
    { value = False
    , ripple = False
    , input = []
    , container = []
    }


{-| Properties for Button options.
-}
type alias Property m =
    Options.Property (Config m) m

{-| Set toggle to ripple when clicked.
-}
ripple : Property m
ripple =
    Internal.option (\options -> { options | ripple = True })


{-| Set toggle to "disabled".
-}
disabled : Property m
disabled =
    Internal.attribute <| Html.Attributes.disabled True


{-| Set toggle value
-}
value : Bool -> Property m
value =
    Internal.option << (\b options -> { options | value = b })


{-| Set radio-button group id. Only one button in the same group can be checked
at a time.
-}
group : String -> Property m
group =
    Options.attribute << Html.Attributes.name



-- VIEW


top : (Msg -> m) -> String -> Model -> Internal.Summary (Config m) m -> List (Html m) -> Html m
top lift kind model summary elems =
    let
        cfg =
            summary.config
    in
        Internal.applyContainer summary
            label
            [ cs ("mdl-" ++ kind)
            , cs ("mdl-js-" ++ kind)
            , cs "mdl-js-ripple-effect" |> when cfg.ripple
            , cs "mdl-js-ripple-effect--ignore-events" |> when cfg.ripple
            , cs "is-upgraded"
            , cs "is-checked" |> when cfg.value
            , cs "is-focused" |> when model.isFocused
            , Internal.on1 "focus" lift (SetFocus True)
            , Internal.on1 "blur" lift (SetFocus False)
            , Internal.attribute <| blurOn "mouseup"
            ]
            (List.concat
                [ elems
                , if cfg.ripple then
                    [ Html.map (Ripple >> lift) <|
                        Ripple.view
                            [ class "mdl-switch__ripple-container mdl-js-ripple-effect mdl-ripple--center" ]
                            model.ripple
                    ]
                  else
                    []
                ]
            )


{-| Component view (checkbox).
-}
viewCheckbox : (Msg -> m) -> Model -> List (Property m) -> List (Html m) -> Html m
viewCheckbox lift model config elems =
    let
        summary =
            Internal.collect defaultConfig config
    in
        [ Internal.applyInput summary
            Html.input
            [ cs "mdl-checkbox__input"
            , Internal.attribute <| type_ "checkbox"
            , Internal.attribute <| checked summary.config.value
            ]
            []
        , span [ class ("mdl-checkbox__label") ] elems
        , span [ class "mdl-checkbox__focus-helper" ] []
        , span
            [ class "mdl-checkbox__box-outline" ]
            [ span
                [ class "mdl-checkbox__tick-outline" ]
                []
            ]
        ]
            |> top lift "checkbox" model summary


{-| Component view (switch)
-}
viewSwitch : (Msg -> m) -> Model -> List (Property m) -> List (Html m) -> Html m
viewSwitch lift model config elems =
    let
        summary =
            Internal.collect defaultConfig config
    in
        [ Internal.applyInput summary
            Html.input
            [ cs "mdl-switch__input"
            , Internal.attribute <| type_ "checkbox"
            , Internal.attribute <| checked summary.config.value
            ]
            []
        , span [ class "mdl-switch__label" ] elems
        , div [ class "mdl-switch__track" ] []
        , div
            [ class "mdl-switch__thumb" ]
            [ span [ class "mdl-switch__focus-helper" ] [] ]
        ]
            |> top lift "switch" model summary


{-| Component view (radio button)
-}
viewRadio : (Msg -> m) -> Model -> List (Property m) -> List (Html m) -> Html m
viewRadio lift model config elems =
    let
        summary =
            Internal.collect defaultConfig config
    in
        [ Internal.applyInput summary
            Html.input
            [ cs "mdl-radio__button"
            , Options.attribute <| type_ "radio"
            , Options.attribute <| checked summary.config.value
            ]
            []
        , span [ class "mdl-radio__label" ] elems
        , span [ class "mdl-radio__outer-circle" ] []
        , span [ class "mdl-radio__inner-circle" ] []
        ]
            |> top lift "radio" model summary



-- COMPONENT


type alias Store s =
    { s | toggles : Indexed Model }


( get, set ) =
    Component.indexed .toggles (\x y -> { y | toggles = x }) defaultModel


{-| Component react function.
-}
react :
    (Component.Msg button textfield menu layout Msg tooltip tabs select dispatch -> m)
    -> Msg
    -> Index
    -> Store s
    -> ( Maybe (Store s), Cmd m )
react =
    Component.react get set Component.TogglesMsg (Component.generalise update)


{-| Component render (checkbox)
-}
checkbox :
    (Component.Msg button textfield menu snackbar Msg tooltip tabs select dispatch -> m)
    -> Component.Index
    -> { a | toggles : Indexed Model }
    -> List (Property m)
    -> List (Html m)
    -> Html m
checkbox =
    Component.render get viewCheckbox Component.TogglesMsg


{-| Component render (switch)
-}
switch :
    (Component.Msg button textfield menu snackbar Msg tooltip tabs select dispatch -> m)
    -> Component.Index
    -> Store s
    -> List (Property m)
    -> List (Html m)
    -> Html m
switch =
    Component.render get viewSwitch Component.TogglesMsg


{-| Component render (radio button)
-}
radio :
    (Component.Msg button textfield menu snackbar Msg tooltip tabs select dispatch -> m)
    -> Component.Index
    -> Store s
    -> List (Property m)
    -> List (Html m)
    -> Html m
radio =
    Component.render get viewRadio Component.TogglesMsg
