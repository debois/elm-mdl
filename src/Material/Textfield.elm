module Material.Textfield
    exposing
        ( Property
        , label
        , floatingLabel
        , error
        , value
        , defaultValue
        , disabled
        , password
        , render
        , react
        , text_
        , textarea
        , rows
        , cols
        , email
        , autofocus
        , maxlength
        , expandable
        , expandableIcon
        , Model
        , defaultModel
        , Msg(..)
        , update
        , view
        )


{-| From the [Material Design Lite documentation](http://www.getmdl.io/components/#textfields-section):

> The Material Design Lite (MDL) text field component is an enhanced version of
> the standard HTML `<input type="text">` and `<input type="textarea">` elements.
> A text field consists of a horizontal line indicating where keyboard input
> can occur and, typically, text that clearly communicates the intended
> contents of the text field. The MDL text field component provides various
> types of text fields, and allows you to add both display and click effects.
>
> Text fields are a common feature of most user interfaces, regardless of a
> site's content or function. Their design and use is therefore an important
> factor in the overall user experience. See the text field component's
> [Material  Design specifications page](https://www.google.com/design/spec/components/text-fields.html)
> for details.
>
> The enhanced text field component has a more vivid visual look than a standard
> text field, and may be initially or programmatically disabled. There are three
> main types of text fields in the text field component, each with its own basic
> coding requirements. The types are single-line, multi-line, and expandable.


Refer to
[this site](https://debois.github.io/elm-mdl/#textfields)
for a live demo.

# Component render
@docs render

# Options
@docs Property, value

## Appearance

@docs label, floatingLabel, error

## Html attributes
@docs disabled, rows, cols
@docs autofocus, maxlength

@docs password, email, textarea, text_
@docs expandable, expandableIcon

# Elm Architecture
@docs Msg, Model, defaultModel, update, view

# Internal use
@docs react

-}

import Html exposing (div, span, Html, text)
import Html.Attributes exposing (class, type_, style)
import Html.Events exposing (targetValue)
import Json.Decode as Decoder
import Material.Component as Component exposing (Index, Indexed)
import Material.Options as Options exposing (cs, css, nop, Style, when)
import Material.Options.Internal as Internal
import Material.Icon as Icon


-- OPTIONS


type Kind
    = Text
    | Textarea
    | Password
    | Email


type alias Config m =
    { labelText : Maybe String
    , labelFloat : Bool
    , error : Maybe String
    , value : Maybe String
    , defaultValue : Maybe String
    , disabled : Bool
    , kind : Kind
    , expandable : Maybe String
    , expandableIcon : String
    , input : List (Options.Style m)
    , container : List (Options.Style m)
    }


defaultConfig : Config m
defaultConfig =
    { labelText = Nothing
    , labelFloat = False
    , error = Nothing
    , value = Nothing
    , defaultValue = Nothing
    , disabled = False
    , kind = Text
    , expandable = Nothing
    , expandableIcon = "search"
    , input = []
    , container = []
    }


{-| Type of Textfield options
-}
type alias Property m =
    Options.Property (Config m) m


{-| Label of the textfield
-}
label : String -> Property m
label =
    Internal.option
        << (\str config -> { config | labelText = Just str })


{-| Label of textfield animates away from the input area on input
-}
floatingLabel : Property m
floatingLabel =
    Internal.option
        (\config -> { config | labelFloat = True })


{-| Specifies the textfield as an `expandable`. The property takes the ID
of the element as parameter as this is currently required.

**NOTE:** When manually setting the **id** of the `input` element using
`Options.inner` then the `expandable` **id** must match
the `input` **id**.
-}
expandable : String -> Property m
expandable id =
    Internal.option
        (\config -> { config | expandable = Just id })


{-| Sets the icon *only* when the expandable has been set to a valid ID.

Defaults to `search`
-}
expandableIcon : String -> Property m
expandableIcon id =
  Internal.option
        (\config -> { config | expandableIcon = id })


{-| Error message
-}
error : String -> Property m
error =
    Internal.option
        << (\str config -> { config | error = Just str })


{-| Current value of the textfield.
-}
value : String -> Property m
value =
    Internal.option
        << (\str config -> { config | value = Just str })


{-| Set the default value of the textfield
-}
defaultValue : String -> Property m
defaultValue =
    Internal.option
        << (\str config -> { config | defaultValue = Just str })


{-| Specifies that the input should automatically get focus when the page loads
-}
autofocus : Property m
autofocus =
    Options.attribute <| Html.Attributes.autofocus True


{-| Specifies the maximum number of characters allowed in the input
-}
maxlength : Int -> Property m
maxlength k =
    Options.attribute <| Html.Attributes.maxlength k


{-| Disable the textfield input
-}
disabled : Property m
disabled =
    Internal.option
        (\config -> { config | disabled = True })


{-| Set properties on the actual `input` element in the Textfield.
-}
input : List (Options.Style m) -> Property m
input =
    Options.input


{-| Sets the type of input to 'email'.
-}
email : Property m
email =
  Internal.option
    (\config -> { config | kind = Email })


{-| Sets the type of input to 'password'.
-}
password : Property m
password =
    Internal.option
        (\config -> { config | kind = Password })


{-| Creates a multiline textarea using 'textarea' element
-}
textarea : Property m
textarea =
    Internal.option
        (\config -> { config | kind = Textarea })


{-| Sets the type of input to 'text'. (Name chosen to avoid clashing with Html.text)
-}
text_ : Property m
text_ =
    Internal.option
        (\config -> { config | kind = Text })


{-| Number of rows in a multi-line input
-}
rows : Int -> Property m
rows k =
    Internal.input [ Options.attribute <| Html.Attributes.rows k ]


{-| Number of columns in a multi-line input
-}
cols : Int -> Property m
cols k =
    Internal.input [ Options.attribute <| Html.Attributes.cols k ]



-- MODEL


{-|
-}
type alias Model =
    { isFocused : Bool
    , isDirty : Bool
    }


{-| Default model. 
-}
defaultModel : Model
defaultModel =
    { isFocused = False
    , isDirty = False
    }



-- ACTIONS, UPDATE


{-| Component actions. `Input` carries the new value of the field.
-}
type Msg
    = Blur
    | Focus
    | Input String


{-| Component update.
-}
update : x -> Msg -> Model -> ( Maybe Model, Cmd msg )
update _ action model =
    (case action of
        Input str ->
            let
                dirty =
                    str /= ""
            in
                if dirty == model.isDirty then
                    Nothing
                else
                    Just { model | isDirty = dirty }

        Blur ->
            Just { model | isFocused = False }

        Focus ->
            Just { model | isFocused = True })
    |> flip (!) [] 



-- VIEW


{-| Component view

Be aware that styling (third argument) is applied to the outermost element
of the textfield's implementation, and so is mostly useful for positioning
(e.g., `margin: 0 auto;` or `align-self: flex-end`). See `Options.input`
if you need to apply styling to the underlying `<input>` element.
-}
view : (Msg -> m) -> Model -> List (Property m) -> x -> Html m
view lift model options _ =
    let
        ({ config } as summary) =
            Internal.collect defaultConfig options

        -- NOTE: These ids need to match the id of the input element
        labelFor =
            case config.expandable of
                Nothing ->
                    []

                Just id ->
                    [ Html.Attributes.for id ]

        expandableId =
            case config.expandable of
                Nothing ->
                    Options.nop

                Just id ->
                    Internal.attribute <| Html.Attributes.id id

        expHolder =
            case config.expandable of
                Nothing ->
                    identity

                Just _ ->
                    (\x ->
                        [ Options.styled_ Html.label
                            [ cs "mdl-button"
                            , cs "mdl-js-button"
                            , cs "mdl-button--icon"
                            ]
                            labelFor
                            [ Icon.i config.expandableIcon ]
                        , Options.styled Html.div
                            [ cs "mdl-textfield__expandable-holder" ]
                            x
                        ]
                    )
    in
        Internal.applyContainer summary
            div
            [ cs "mdl-textfield"
            , cs "mdl-js-textfield"
            , cs "is-upgraded"
            , Internal.on1 "focus" lift Focus
            , Internal.on1 "blur" lift Blur
            , cs "mdl-textfield--floating-label" |> when config.labelFloat
            , cs "is-invalid" |> when (config.error /= Nothing)
            , cs "is-dirty"
                |> when
                    (case config.value of
                        Just "" ->
                            False

                        Just _ ->
                            True

                        Nothing ->
                            model.isDirty
                    )
            , cs "is-focused" |> when (model.isFocused && not config.disabled)
            , cs "is-disabled" |> when config.disabled
            , cs "mdl-textfield--expandable" |> when (config.expandable /= Nothing)
            ]
        <|
            expHolder
                [ Internal.applyInput summary
                    (if config.kind == Textarea then
                        Html.textarea
                     else
                        Html.input
                    )
                    [ cs "mdl-textfield__input"
                    , css "outline" "none"
                    , Internal.on1 "focus" lift Focus
                    , Internal.on1 "blur" lift Blur
                    , case config.kind of
                        Text ->
                            Internal.attribute <| type_ "text"

                        Password ->
                            Internal.attribute <| type_ "password"

                        Email ->
                            Internal.attribute <| type_ "email"

                        _ ->
                            nop
                    , Internal.attribute (Html.Attributes.disabled True)
                        |> when config.disabled
                    , expandableId
                    , case config.value of
                        Nothing ->
                            -- If user is not setting value, is we need the default input
                            -- decoder to maintain is-dirty
                            Options.on "input"
                                (Decoder.map (Input >> lift) Html.Events.targetValue)

                        Just v ->
                            Internal.attribute <| Html.Attributes.value v
                    , case config.defaultValue of
                        Nothing ->
                            Options.nop

                        Just v ->
                            Internal.attribute <| Html.Attributes.defaultValue v
                    ]
                    []
                , Html.label
                    ([ class "mdl-textfield__label" ] ++ labelFor)
                    (case config.labelText of
                        Just str ->
                            [ text str ]

                        Nothing ->
                            []
                    )
                , config.error
                    |> Maybe.map (\e -> span [ class "mdl-textfield__error" ] [ text e ])
                    |> Maybe.withDefault (div [] [])
                ]



-- COMPONENT


type alias Store s =
    { s | textfield : Indexed Model }


( get, set ) =
    Component.indexed .textfield (\x c -> { c | textfield = x }) defaultModel


{-| Component react function.
-}
react
    : ( Component.Msg button Msg menu layout toggles tooltip tabs dispatch -> msg)
    -> Msg
    -> Index
    -> Store s
    -> ( Maybe (Store s), Cmd msg )
react =
    Component.react get
        set
        Component.TextfieldMsg update


{-| Component render. Below is an example, assuming boilerplate setup as indicated
  in `Material`, and a user message `ChangeAgeMsg Int`.

    Textfield.render Mdl [0] model.mdl
      [ Textfield.label "Age"
      , Textfield.floatingLabel
      , Textfield.value model.age
      , Options.onInput (String.toInt >> ChangeAgeMsg)
      ]

Be aware that styling (third argument) is applied to the outermost element
of the textfield's implementation, and so is mostly useful for positioning
(e.g., `margin: 0 auto;` or `align-self: flex-end`). See `Textfield.style`
if you need to apply styling to the underlying `<input>` element.
-}
render
    : (Component.Msg button Msg menu layout toggles tooltip tabs dispatch -> m)
    -> Index
    -> Store s
    -> List (Property m)
    -> x
    -> Html m       
render =
    Component.render get view Component.TextfieldMsg
