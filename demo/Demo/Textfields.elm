module Demo.Textfields exposing (model, Model, update, view, Msg)

import Html exposing (..)
import Platform.Cmd exposing (Cmd)
import Regex
import Json.Decode as Decoder
import String

import Material.Textfield as Textfield
import Material.Grid as Grid exposing (..)
import Material.Options as Options exposing (css)
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
  , focus5 : Bool
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
  , focus5 = False
  , str9 = "Try selecting within this text"
  , selection = { begin = -1, end = -1 }
  }


-- ACTION, UPDATE


type Msg
  = MDL (Material.Msg Msg)
  | Upd0 String
  | Upd3 String
  | Upd4 String
  | Upd6 String
  | Upd9 String
  | SetFocus5 Bool 
  | Slider Float
  | SelectionChanged Selection


selectionDecoder : Decoder.Decoder Msg
selectionDecoder =
  Decoder.map SelectionChanged
    <| Decoder.object2 Selection
         (Decoder.at ["target", "selectionStart"] Decoder.int)
         (Decoder.at ["target", "selectionEnd"] Decoder.int)
  

pure : Model -> Maybe (Model, Cmd Msg)
pure = 
  flip (,) Cmd.none >> Just


update : Msg -> Model -> Maybe (Model, Cmd Msg)
update action model =
  case action of
    MDL action' ->
      Material.update action' model |> Just

    Upd0 str ->
      { model | str0 = str } |> pure

    Upd3 str ->
      { model | str3 = str } |> pure

    Upd4 str ->
      { model | str4 = str } |> pure

    Upd6 str ->
      { model | str6 = str } |> pure

    Upd9 str ->
      { model | str9 = str } |> pure

    Slider value ->
      { model | length = value } |> pure

    SetFocus5 x ->
      { model | focus5 = x } |> pure

    SelectionChanged selection ->
      -- High-frequency event; return referentially equal model on NOP. 
      if selection == model.selection then
        Nothing
      else 
        ( { model | selection = selection }, Cmd.none )
          |> Just

-- VIEW


type alias Mdl =
  Material.Model


rx : String
rx =
  "[0-9]*"


rx' : Regex.Regex
rx' =
  Regex.regex rx


{- Check that rx matches all of str.
-}
match : String -> Regex.Regex -> Bool
match str rx =
  Regex.find Regex.All rx str
    |> List.any (.match >> (==) str)


textfields : Model -> List (String, Html Msg, String) 
textfields model = 
  [ ( "Basic textfield"
    , Textfield.render MDL [0] model.mdl
        [ Textfield.onInput Upd0 ]
    , """
        Textfield.render MDL [0] model.mdl
          [ Textfield.onInput Upd0 ]
       """
    )

  , ( "Labelled textfield"
    , Textfield.render MDL [1] model.mdl
        [ Textfield.label "Labelled" ]
    , """
       Textfield.render MDL [1] model.mdl
         [ Textfield.label "Labelled" ]
       """
    )
  , ( "Labelled textfield, floating label"
    , Textfield.render MDL [2] model.mdl
        [ Textfield.label "Floating label"
        , Textfield.floatingLabel
        , Textfield.text'
        ]
    , """
        Textfield.render MDL [2] model.mdl
          [ Textfield.label "Floating label"
          , Textfield.floatingLabel
          , Textfield.text'
          ]
       """
    )
  , ( "Disabled textfield"
    , Textfield.render MDL [3] model.mdl
        [ Textfield.label "Disabled"
        , Textfield.disabled
        , Textfield.value <|
            model.str0
            ++ if model.str0 /= "" then " (still disabled, though)" else ""
        ]
    , """
      Textfield.render MDL [3] model.mdl
        [ Textfield.label "Disabled"
        , Textfield.disabled
        , Textfield.value <|
            model.str0
            ++ if model.str0 /= "" then
                " (still disabled, though)"
               else ""
        ]
       """
    )
  , ( "Textfield with error checking"
    , Textfield.render MDL [4] model.mdl
        [ Textfield.label "w/error checking"
        , if not <| match model.str4 rx' then
            Textfield.error <| "Doesn't match " ++ rx
          else
            Options.nop
        , Textfield.onInput Upd4
        ]
    , """
    Textfield.render MDL [4] model.mdl
      [ Textfield.label "w/error checking"
      , if not <| match model.str4 rx' then
          Textfield.error <| "Doesn't match " ++ rx
        else
          Options.nop
      , Textfield.onInput Upd4
      ]
       """
    )
  , ( "Textfield for passwords"
    , Textfield.render MDL [5] model.mdl
        [ Textfield.label "Enter password"
        , Textfield.floatingLabel
        , Textfield.password
        ]
    , """
      Textfield.render MDL [5] model.mdl
        [ Textfield.label "Enter password"
        , Textfield.floatingLabel
        , Textfield.password
        ]
       """
    )
  , ( "Multi-line textfield"
    , Textfield.render MDL [6] model.mdl
        [ Textfield.label "Default multiline textfield"
        , Textfield.textarea
        ]
    , """
      Textfield.render MDL [6] model.mdl
        [ Textfield.label "Default multiline textfield"
        , Textfield.textarea
        ]
       """
    )

  , ( "Multi-line textfield, 6 rows"
    , Textfield.render MDL [7] model.mdl
        [ Textfield.label "Multiline with 6 rows"
        , Textfield.floatingLabel
        , Textfield.textarea
        , Textfield.rows 6
        ]
    , """
      Textfield.render MDL [7] model.mdl
        [ Textfield.label "Multiline with 6 rows"
        , Textfield.floatingLabel
        , Textfield.textarea
        , Textfield.rows 6
        ]
       """
    )

  , ( "Multi-line textfield with character limit (focused: " ++ (toString model.focus5) ++ ")"
    , Html.div []
    [ Textfield.render MDL [8] model.mdl
        [ Textfield.label ("Multiline textfield (" ++
                            (toString (String.length model.str6))
                            ++ " of " ++ (toString (truncate model.length))
                            ++ " char limit)")
        , Textfield.onInput Upd6
        , Textfield.textarea
        , Textfield.maxlength (truncate model.length)
        , Textfield.autofocus
        , Textfield.floatingLabel
        , Textfield.onFocus (SetFocus5 True)
        , Textfield.onBlur (SetFocus5 False)
        ]
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
        --, Textfield.error <| if model.focus5 then "focused" else "not focused"
        ]
    ]
    , """
       Textfield.render MDL [8] model.mdl
         [ Textfield.label
             ("Multiline textfield (" ++
                (toString (String.length model.str6))
                ++ " of " ++ (toString (truncate model.length))
                ++ " char limit)")
         , Textfield.onInput Upd6
         , Textfield.textarea
         , Textfield.maxlength (truncate model.length)
         , Textfield.autofocus
         , Textfield.floatingLabel
         , Textfield.onFocus (SetFocus5 True)
         , Textfield.onBlur (SetFocus5 False)
         ]
       """
    )
  ]


custom : Model -> List (String, Html Msg, String)
custom model = 
  [ ( "Custom event handling"
    , Html.div
        []
        [ Textfield.render MDL [9] model.mdl
            [ Textfield.label "Custom event handling"
            , Textfield.textarea
            , Textfield.onInput Upd9
            , Textfield.value model.str9
            , Textfield.on "keyup" selectionDecoder
            , Textfield.on "mousemove" selectionDecoder
            , Textfield.on "click" selectionDecoder
        ]
        , Options.styled Html.p
            [ css "width" "300px"
            , css "word-wrap" "break-word"
            ]
            [ text <| "Selected text: " ++ 
                String.slice model.selection.begin model.selection.end model.str9
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
          [ Textfield.render MDL [9] model.mdl
              [ Textfield.label "Custom event handling"
              , Textfield.textarea
              , Textfield.onInput Input
              , Textfield.on "keyup" selectionDecoder
              , Textfield.on "mousemove" selectionDecoder
              , Textfield.on "click" selectionDecoder
              ]
            , [ text <| "Selected text: " ++ 
                  String.slice model.selection.begin model.selection.end model.value
              ]
     """
     )
   ]


view1 : ( String, Html Msg, String ) -> Cell Msg
view1 (header, html, code) = 
        cell
          [size Phone 4, size Tablet 6, offset Tablet 1, size Desktop 8, offset Desktop 2]
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
    Page.body1' "Textfields" srcUrl intro references [demo1] [demo2]



intro : Html a
intro = Page.fromMDL "http://www.getmdl.io/components/#textfields-section" """
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


references : List (String, String)
references =
  [ Page.package "http://package.elm-lang.org/packages/debois/elm-mdl/latest/Material-Textfield"
  , Page.mds "https://www.google.com/design/spec/components/text-fields.html"
  , Page.mdl "https://www.getmdl.io/components/#textfields-section"
  ]
