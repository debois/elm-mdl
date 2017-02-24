module Demo.Textfields exposing (model, Model, update, view, Msg)

import Html exposing (..)
import Regex
import Json.Decode as Decoder
import String
import Dom
import Task
import Material.Textfield as Textfield
import Material.Grid as Grid exposing (..)
import Material.Options as Options exposing (css)
import Material.Button as Button
import Material
import Material.Slider as Slider
import Material.Typography as Typo
import Demo.Page as Page
import Demo.Code as Code


-- MODEL


type alias Selection =
    { begin : Int
    , end : Int
    }


type alias Model =
    { mdl : Material.Model
    , str0 : String
    , str3 : String
    , str4 : String
    , str6 : String
    , length : Float
    , focus : Bool
    , str9 : String
    , selection : Selection
    }


model : Model
model =
    { mdl = Material.model
    , str0 = ""
    , str3 = ""
    , str4 = ""
    , str6 = ""
    , length = 5
    , focus = False
    , str9 = "Try selecting within this text"
    , selection = { begin = -1, end = -1 }
    }



-- ACTION, UPDATE


type Msg
    = Mdl (Material.Msg Msg)
    | Upd0 String
    | Upd3 String
    | Upd4 String
    | Upd6 String
    | Upd9 String
    | SetFocus Bool
    | Focus
    | Slider Float
    | SelectionChanged Selection
    | Nop


selectionDecoder : Decoder.Decoder Msg
selectionDecoder =
    Decoder.map SelectionChanged <|
        Decoder.map2 Selection
            (Decoder.at [ "target", "selectionStart" ] Decoder.int)
            (Decoder.at [ "target", "selectionEnd" ] Decoder.int)


update : Msg -> Model -> Maybe ( Model, Cmd Msg )
update msg model =
    case msg of
        Nop ->
            Nothing

        SelectionChanged selection ->
            -- High-frequency event; return referentially equal model on NOP.
            if selection == model.selection then
                Nothing
            else
                ( { model | selection = selection }, Cmd.none )
                    |> Just

        Focus ->
            ( model
            , Task.attempt (\_ -> Nop) <| Dom.focus "my-textfield"
            )
                |> Just

        Mdl msg_ ->
            Material.update Mdl msg_ model |> Just

        Upd0 str ->
            { model | str0 = str }
                ! []
                |> Just

        Upd3 str ->
            { model | str3 = str }
                ! []
                |> Just

        Upd4 str ->
            { model | str4 = str }
                ! []
                |> Just

        Upd6 str ->
            { model | str6 = str }
                ! []
                |> Just

        Upd9 str ->
            { model | str9 = str }
                ! []
                |> Just

        Slider value ->
            { model | length = value }
                ! []
                |> Just

        SetFocus x ->
            { model | focus = x }
                ! []
                |> Just



-- VIEW


rx : String
rx =
    "[0-9]*"


rx_ : Regex.Regex
rx_ =
    Regex.regex rx



{- Check that rx matches all of str. -}


match : String -> Regex.Regex -> Bool
match str rx =
    Regex.find Regex.All rx str
        |> List.any (.match >> (==) str)


textfields : Model -> List ( String, Html Msg, String )
textfields model =
    [ ( "Basic textfield"
      , Textfield.render Mdl
            [ 0 ]
            model.mdl
            [ Options.onInput Upd0
            , Textfield.value model.str0
            ]
            []
      , """
        Textfield.render Mdl [0] model.mdl
          [ Options.onInput Upd0
          , Textfield.value model.str0
          ]
          []
       """
      )
    , ( "Labelled textfield"
      , Textfield.render Mdl
            [ 1 ]
            model.mdl
            [ Textfield.label "Labelled" ]
            []
      , """
       Textfield.render Mdl [1] model.mdl
         [ Textfield.label "Labelled" ]
         []
       """
      )
    , ( "Labelled textfield, floating label"
      , Textfield.render Mdl
            [ 2 ]
            model.mdl
            [ Textfield.label "Floating label"
            , Textfield.floatingLabel
            , Textfield.text_
            ]
            []
      , """
        Textfield.render Mdl [2] model.mdl
          [ Textfield.label "Floating label"
          , Textfield.floatingLabel
          , Textfield.text_
          ]
          []
       """
      )
    , ( "Disabled textfield"
      , Textfield.render Mdl
            [ 3 ]
            model.mdl
            [ Textfield.label "Disabled"
            , Textfield.disabled
            , Textfield.value <|
                model.str0
                    ++ if model.str0 /= "" then
                        " (still disabled, though)"
                       else
                        ""
            ]
            []
      , """
      Textfield.render Mdl [3] model.mdl
        [ Textfield.label "Disabled"
        , Textfield.disabled
        , Textfield.value <|
            model.str0
            ++ if model.str0 /= "" then
                " (still disabled, though)"
               else ""
        ]
        []
       """
      )
    , ( "Textfield with error checking"
      , Textfield.render Mdl
            [ 4 ]
            model.mdl
            [ Textfield.label "w/error checking"
            , Textfield.error ("Doesn't match " ++ rx)
                |> Options.when (not <| match model.str4 rx_)
            , Options.onInput Upd4
            , Textfield.value model.str4
            ]
            []
      , """
    Textfield.render Mdl [4] model.mdl
      [ Textfield.label "w/error checking"
      , Options.onInput Upd4
      , Textfield.error ("Doesn't match " ++ rx)
          |> Options.when (not <| match model.str4 rx_)
      ]
      []
       """
      )
    , ( "Textfield for passwords"
      , Textfield.render Mdl
            [ 5 ]
            model.mdl
            [ Textfield.label "Enter password"
            , Textfield.floatingLabel
            , Textfield.password
            ]
            []
      , """
      Textfield.render Mdl [5] model.mdl
        [ Textfield.label "Enter password"
        , Textfield.floatingLabel
        , Textfield.password
        ]
        []
       """
      )
    , ( "Textfield for email"
      , Textfield.render Mdl
            [ 6 ]
            model.mdl
            [ Textfield.label "Enter email"
            , Textfield.floatingLabel
            , Textfield.email
            ]
            []
      , """
      Textfield.render Mdl [6] model.mdl
        [ Textfield.label "Enter email"
        , Textfield.floatingLabel
        , Textfield.email
        []
       """
      )
    , ( "Expandable textfield"
      , Textfield.render Mdl
            [ 7 ]
            model.mdl
            [ Textfield.label "Expandable"
            , Textfield.floatingLabel
            , Textfield.expandable "id-of-expandable-1"
            , Textfield.expandableIcon "search"
            ]
            []
      , """
      Textfield.render Mdl [7] model.mdl
        [ Textfield.label "Expandable"
        , Textfield.floatingLabel
        , Textfield.expandable "id-of-expandable-1"
        , Textfield.expandableIcon "search"
        ]
        []
       """
      )
    , ( "Multi-line textfield"
      , Textfield.render Mdl
            [ 8 ]
            model.mdl
            [ Textfield.label "Default multiline textfield"
            , Textfield.textarea
            ]
            []
      , """
      Textfield.render Mdl [8] model.mdl
        [ Textfield.label "Default multiline textfield"
        , Textfield.textarea
        ]
        []
       """
      )
    , ( "Multi-line textfield, 6 rows"
      , Textfield.render Mdl
            [ 9 ]
            model.mdl
            [ Textfield.label "Multiline with 6 rows"
            , Textfield.floatingLabel
            , Textfield.textarea
            , Textfield.rows 6
            ]
            []
      , """
      Textfield.render Mdl [9] model.mdl
        [ Textfield.label "Multiline with 6 rows"
        , Textfield.floatingLabel
        , Textfield.textarea
        , Textfield.rows 6
        ]
        []
       """
      )
    , ( "Multi-line textfield with character limit"
      , Html.div []
            [ Textfield.render Mdl
                [ 10 ]
                model.mdl
                [ Textfield.label
                    ("Multiline textfield ("
                        ++ (toString (String.length model.str6))
                        ++ " of "
                        ++ (toString (truncate model.length))
                        ++ " char limit)"
                    )
                , Options.onInput Upd6
                , Textfield.value model.str6
                , Textfield.textarea
                , Textfield.maxlength (truncate model.length)
                , Textfield.floatingLabel
                ]
                []
            , Options.styled Html.p
                [ Options.css "width" "80%" ]
                [ Options.span
                    [ Typo.caption ]
                    [ Html.text "Drag to change the maxlength" ]
                , Slider.view
                    [ Slider.onChange Slider
                    , Slider.value model.length
                    , Slider.max 100
                    , Slider.min 1
                    , Slider.step 1
                    ]
                ]
            ]
      , """
       Textfield.render Mdl [10] model.mdl
         [ Textfield.label
             ("Multiline textfield (" ++
                (toString (String.length model.str6))
                ++ " of " ++ (toString (truncate model.length))
                ++ " char limit)")
         , Options.onInput Upd6
         , Textfield.value model.str6
         , Textfield.textarea
         , Textfield.maxlength (truncate model.length)
         , Textfield.floatingLabel
         ]
       """
      )
    ]


custom : Model -> List ( String, Html Msg, String )
custom model =
    [ ( "Working with focus"
      , Options.div
            [ css "display" "flex"
            , css "flex-direction" "column"
            , css "align-items" "flex-start"
            ]
            [ Textfield.render Mdl
                [ 11 ]
                model.mdl
                [ Textfield.floatingLabel
                , Textfield.label <|
                    if model.focus then
                        "Focused"
                    else
                        "Not focused"
                , Options.onFocus (SetFocus True)
                , Options.onBlur (SetFocus False)
                , Options.id "my-textfield"
                ]
                []
            , Button.render Mdl
                [ 12 ]
                model.mdl
                [ Options.onClick Focus
                , Button.colored
                , css "align-self" "flex-end"
                , Button.disabled |> Options.when model.focus
                ]
                [ text "Focus" ]
            ]
      , """
       [ Textfield.render Mdl [11] model.mdl
          [ Textfield.floatingLabel
          , Textfield.label <| if model.focus then "Focused" else "Not focused"
          , Options.onFocus (SetFocus True)
          , Options.onBlur (SetFocus False)
          , Options.id "my-textfield"
          ]
          []
      , Button.render Mdl [12] model.mdl
          [ Options.onClick Focus
          , Button.colored
          , css "align-self" "flex-end"
          , Button.disabled `Options.when` model.focus
          ]
          [ text "Focus" ]
       ]
          """
      )
    , ( "Working with selection"
      , Html.div
            []
            [ Textfield.render Mdl
                [ 13 ]
                model.mdl
                [ Textfield.label "Custom event handling"
                , Textfield.textarea
                , Textfield.value model.str9
                , Options.onInput Upd9
                , Options.on "keyup" selectionDecoder
                , Options.on "mousemove" selectionDecoder
                , Options.on "click" selectionDecoder
                ]
                []
            , Options.styled Html.p
                [ css "width" "300px"
                , css "word-wrap" "break-word"
                ]
                [ text <|
                    "Selected text: "
                        ++ String.slice model.selection.begin model.selection.end model.str9
                ]
            ]
      , """
      type alias Selection =
        { begin : Int
        , end : Int
        }


      type alias Model =
        { value : String
        , selection : Selection
        }


      type Msg =
        ...
        | SelectionChanged Selection
        | Input String


      update msg model =
        case msg of
          ...
          | Selection selection ->
              {- This clause is triggered by the high-frequency mousemove
              event. When the selection didn't change, we make sure to
              return an unchanged model so that Html.Lazy can kick in and
              prevent unnecessary re-renders.
              -}
              if model.selection == selection then
                ( model, Cmd.none )
              else
                ( { model | selection = selection }, Cmd.none )

          | Input str ->
              ( { model | value = str }, Cmd.none )


      selectionDecoder : Decoder.Decoder Msg
      selectionDecoder =
        Decoder.object2 Selection
          (Decoder.at ["target", "selectionStart"] Decoder.int)
          (Decoder.at ["target", "selectionEnd"] Decoder.int)


      view : Model -> Html Msg
      view model =
        div []
          [ Textfield.render Mdl [13] model.mdl
              [ Textfield.label "Custom event handling"
              , Textfield.textarea
              , Options.onInput Input
              , Options.on "keyup" selectionDecoder
              , Options.on "mousemove" selectionDecoder
              , Options.on "click" selectionDecoder
              ]
              []
            , [ text <| "Selected text: " ++
                  String.slice model.selection.begin model.selection.end model.value
              ]
     """
      )
    ]


view1 : ( String, Html Msg, String ) -> Cell Msg
view1 ( header, html, code ) =
    cell
        [ size Phone 4, size Tablet 6, offset Tablet 1, size Desktop 8, offset Desktop 2 ]
        [ h4 [] [ text header ]
        , Options.div
            [ Options.center ]
            [ html ]
        , Code.code [ css "margin" "24px 0" ] code
        ]


view : Model -> Html Msg
view model =
    let
        demo1 =
            grid [] (List.map view1 <| textfields model)

        demo2 =
            grid [] (List.map view1 <| custom model)
    in
        Page.body1_ "Textfields" srcUrl intro references [ demo1 ] [ demo2 ]


intro : Html a
intro =
    Page.fromMDL "http://www.getmdl.io/components/#textfields-section" """
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
"""


srcUrl : String
srcUrl =
    "https://github.com/debois/elm-mdl/blob/master/demo/Demo/Textfields.elm"


references : List ( String, String )
references =
    [ Page.package "http://package.elm-lang.org/packages/debois/elm-mdl/latest/Material-Textfield"
    , Page.mds "https://www.google.com/design/spec/components/text-fields.html"
    , Page.mdl "https://www.getmdl.io/components/#textfields-section"
    ]
