module Demo.Menus exposing (model, Model, view, update, Msg(Mdl))

import Demo.Code as Code
import Demo.Page as Page
import Html.Attributes exposing (href)
import Html exposing (Html, text, p, a)
import Material
import Material.Color as Color
import Material.Elevation as Elevation
import Material.Grid as Grid
import Material.Icon as Icon
import Material.Menu as Menu
import Material.Options as Options exposing (cs, css, div, nop, when)
import Material.Dropdown.Item as Item
import Material.Textfield as Textfield
import Set exposing (Set)
import String


-- MODEL


type alias Model =
    { mdl : Material.Model
    , selected : Maybe ( Int, Int )
    , checked : Set Int
    , icon : Maybe String
    }


model : Model
model =
    { mdl = Material.model
    , selected = Nothing
    , checked = Set.fromList [ 0, 2 ]
    , icon = Nothing
    }



-- ACTION, UPDATE


type Msg
    = Mdl (Material.Msg Msg)
    | Select Int Int
    | SetIcon String
    | Flip Int
    | Nop


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Mdl action_ ->
            Material.update Mdl action_ model

        Select menu item ->
            ( { model | selected = Just ( menu, item ) }
            , Cmd.none
            )

        SetIcon s ->
            ( { model
                | icon =
                    if s == "" then
                        Nothing
                    else
                        Just s
              }
            , Cmd.none
            )

        Flip k ->
            ( { model
                | checked =
                    if Set.member k model.checked then
                        Set.remove k model.checked
                    else
                        Set.insert k model.checked
              }
            , Cmd.none
            )

        Nop ->
            ( model, Cmd.none )



-- VIEW


type alias Menu =
    { menu : Html Msg
    , title : String
    , code : String
    , comment : String
    }


type alias Align =
    ( String, Menu.Property Msg )


options : Model -> Align -> List (Menu.Property Msg)
options model align =
    [ model.icon
        |> Maybe.map Menu.icon
        |> Maybe.withDefault nop
    , Tuple.second align
    ]


showOptions : Model -> Align -> String
showOptions model align =
    let
        inner =
            [ model.icon
                |> Maybe.map (\i -> "Menu.icon \"" ++ i ++ "\"")
                |> Maybe.withDefault ""
            , "Menu." ++ Tuple.first align
            ]
                |> List.filter ((/=) "")
                |> String.join ", "
    in
        if inner == "" then
            "[]"
        else
            "[ " ++ inner ++ " ]"


basic : Model -> Align -> Menu
basic model align =
    { title = "Basic "
    , menu =
        Menu.render Mdl
            [ 0 ]
            model.mdl
            (options model align)
            ( let
                  item options html =
                      Item.item (Item.ripple::options) html
              in
                [ item
                    [ Item.onSelect (Select 0 0) ]
                    [ text "English (US)" ]
                , item
                    [ Item.onSelect (Select 0 1) ]
                    [ text "français" ]
                , item
                    [ Item.onSelect (Select 0 2) ]
                    [ text "中文" ]
                ]
            )
    , code = """
      Menu.render Mdl [0] model.mdl
        """ ++ showOptions model align ++ """
        [ Item.item
            [ Item.onSelect MySelectMsg0 ]
            [ text "English (US)" ]
        , Item.item
            [ Item.onSelect MySelectMsg1 ]
            [ text "français" ]
        , Item.item
            [ Item.onSelect MySelectMsg2 ]
            [ text "中文" ]
        ]
        """
    , comment =
        ""
    }


edit : Model -> Align -> Menu
edit model align =
    { title = "Disabled items and dividers"
    , menu =
        Menu.render Mdl
            [ 1 ]
            model.mdl
            (options model align)
            [ Item.item
                [ Item.onSelect (Select 1 0) ]
                [ text "Undo" ]
            , Item.item
                [ Item.onSelect (Select 1 1)
                , Item.divider
                , Item.disabled
                ]
                [ text "Redo" ]
            , Item.item
                [ Item.disabled ]
                [ text "Cut" ]
            , Item.item
                [ Item.disabled ]
                [ text "Copy" ]
            , Item.item
                [ Item.onSelect (Select 1 4) ]
                [ text "Paste" ]
            ]
    , code = """
          """
    , comment =
        """It doesn't matter whether or not you supply an `onSelect` handler to a
           `disabled` item. Even if you do, clicks won't register."""
    }


icons : Model -> Align -> Menu
icons model align =
    { title = "Icons"
    , menu =
        let
            i name =
                Icon.view name [ css "width" "40px" ]

            padding =
                css "padding-right" "24px"
        in
            Menu.render Mdl
                [ 2 ]
                model.mdl
                (options model align)
                [ Item.item
                    [ Item.onSelect (Select 1 0), padding ]
                    [ i "remove_red_eye", text "Preview" ]
                , Item.item
                    [ Item.onSelect (Select 1 1), padding ]
                    [ i "person_add", text "Share" ]
                , Item.item
                    [ Item.onSelect (Select 1 2), padding, Item.divider ]
                    [ i "link", text "Get link" ]
                , Item.item
                    [ Item.onSelect (Select 1 3), padding ]
                    [ i "delete", text "Remove" ]
                ]
    , code = """
      let
        i name =
          Icon.view name [ css "width" "40px" ]
        padding =
          css "padding-right" "24px"
      in
        Menu.render Mdl [2] model.mdl
          """ ++ showOptions model align ++ """
          [ Item.item
              [ Item.onSelect MySelectMsg0, padding ]
              [ i "remove_red_eye", text "Preview" ]
          , Item.item
              [ Item.onSelect MySelectMsg1, padding ]
              [ i "person_add", text "Share" ]
          , Item.item
              [ Item.onSelect MySelectMsg2, padding, Item.divider ]
              [ i "link", text "Get link" ]
          , Item.item
              [ Item.onSelect MySelectMsg3, padding ]
              [ i "delete", text "Remove" ]
          ]"""
    , comment =
        """Note that the item right padding has been increased by 8px to
         visually accomodate the perceived extra space to the left of
         the icons.
      """
    }


checkmarks : Model -> Align -> Menu
checkmarks model align =
    let
        isChecked k =
            Set.member k model.checked

        checkmark x =
            if x then
                Icon.view "check" [ css "width" "40px" ]
            else
                Options.span [ css "width" "40px" ] []
    in
        { title = "Checkmarks"
        , menu =
            Menu.render Mdl
                [ 3 ]
                model.mdl
                (options model align)
                [ Item.item
                    [ Item.onSelect (Flip 0) ]
                    [ checkmark (isChecked 0), text "Grid lines" ]
                , Item.item
                    [ Item.onSelect (Flip 1) ]
                    [ checkmark (isChecked 1), text "Page breaks" ]
                , Item.item
                    [ Item.onSelect (Flip 2) ]
                    [ checkmark (isChecked 2), text "Rules" ]
                ]
        , code = """
        let
          checkmark x =
            if x then
              Icon.view "check" [ css "width" "40px" ]
            else
              Options.span [ css "width" "40px" ] []
        in
          Menu.render Mdl [3] model.mdl
            """ ++ showOptions model align ++ """
            [ Item.item
                [ Item.onSelect MySelectMsg0
                [ checkmark """ ++ (toString <| isChecked 0) ++ """, text "Grid lines" ]
            , Item.item
                [ Item.onSelect MySelectMsg1 ]
                [ checkmark """ ++ (toString <| isChecked 1) ++ """, text "Page breaks" ]
            , Item.item
                [ Item.onSelect MySelectMsg2 ]
                [ checkmark """ ++ (toString <| isChecked 2) ++ """, text "Rules" ]
            ]"""
        , comment =
            """ Note the explicit width on `Icon.view` and `Options.span`, which
            causes icons to be neatly aligned.
        """
        }


view1 : Model -> Int -> Menu -> List (Html Msg)
view1 model idx { title, menu, code, comment } =
    let
        bar idx rightAlign =
            div
                [ css "box-sizing" "border-box"
                , css "width" "100%"
                , css "padding" "16px"
                , css "height" "64px"
                , Color.background Color.accent
                , Color.text Color.accentContrast
                ]
                [ div
                    [ css "box-sizing" "border-box"
                    , css "position" "absolute"
                    , css
                        (if rightAlign then
                            "right"
                         else
                            "left"
                        )
                        "16px"
                    ]
                    [ menu ]
                ]

        background =
            div
                [ cs "background"
                , css "height" "148px"
                , css "width" "100%"
                , Color.background Color.white
                ]
                []
    in
        [ Html.h4 [] [ text title ]
        , div
            [ css "display" "flex"
            , css "flex-flow" "row wrap"
            , css "width" "100%"
            ]
            [ div
                [ css "width" "328px"
                , css "display" "flex"
                , css "flex-direction" "column"
                , css "justify-content" "space-between"
                ]
                [ div
                    [ Elevation.e2
                    , css "margin" "0 64px 64px 64px"
                    , css "width" "200px"
                    ]
                    [ div
                        [ css "position" "relative"
                        , css "width" "200px"
                        ]
                        (if idx > 1 then
                            [ background, bar idx (idx % 2 == 1) ]
                         else
                            [ bar idx (idx % 2 == 1), background ]
                        )
                    ]
                , div [ css "flex-grow" "1" ] []
                , div
                    [ css "margin-right" "64px"
                    , css "align-self" "flex-end"
                    ]
                    [ Options.styled p [ css "text-align" "right" ] [ text comment ] ]
                ]
            , Code.code [ css "flex-grow" "1" ] code
            ]
        ]


demo1 : Model -> List (Html Msg)
demo1 model =
    [ basic model ( "bottomLeft", Menu.bottomLeft )
    , edit model ( "bottomRight", Menu.bottomRight )
    , checkmarks model ( "topLeft", Menu.topLeft )
    , icons model ( "topRight", Menu.topRight )
    ]
        |> List.indexedMap (view1 model)
        |> List.concatMap identity


view : Model -> Html Msg
view model =
    let
        demo2 =
            [ Grid.grid
                []
                [ Grid.cell
                    [ Grid.size Grid.All 4
                    ]
                    [{- model.selected
                        |> Maybe.map (\i -> "You chose item '" ++ i ++ "'")
                        |> Maybe.withDefault ""
                        |> text
                     -}
                    ]
                , Grid.cell
                    [ Grid.size Grid.All 4
                    , Grid.offset Grid.Desktop 4
                    ]
                    [ Textfield.render Mdl
                        [ 2 ]
                        model.mdl
                        [ Textfield.label "Menu icon"
                        , Textfield.floatingLabel
                        , model.icon
                            |> Maybe.withDefault ""
                            |> Textfield.value
                        , Options.onInput SetIcon
                        ]
                        []
                    , Options.styled p
                        [ css "margin-top" "1rem" ]
                        [ text "Try 'list' or 'menu', or refer to "
                        , a [ href "https://design.google.com/icons/" ]
                            [ text " the Material Icon library" ]
                        , text " for possible values. Replace spaces (' ') with underscores ('_')."
                        ]
                    ]
                ]
            ]
    in
        Page.body1_ "Menus" srcUrl intro references (demo1 model) demo2


intro : Html m
intro =
    Page.fromMDL "https://www.getmdl.io/components/#menus-section" """

> The Material Design Lite (MDL) menu component is a user interface element
> that allows users to select one of a number of options. The selection
> typically results in an action initiation, a setting change, or other
> observable effect. Menu options are always presented in sets of two or more,
> and options may be programmatically enabled or disabled as required. The menu
> appears when the user is asked to choose among a series of options, and is
> usually dismissed after the choice is made.
>
> Menus are an established but non-standardized feature in user interfaces, and
> allow users to make choices that direct the activity, progress, or
> characteristics of software. Their design and use is an important factor in
> the overall user experience. See the menu component's <a href="http://www.google.com/design/spec/components/menus.html">Material Design
> specifications page</a> for details.

"""


srcUrl : String
srcUrl =
    "https://github.com/debois/elm-mdl/blob/master/demo/Demo/Menus.elm"


references : List ( String, String )
references =
    [ Page.package "http://package.elm-lang.org/packages/debois/elm-mdl/latest/Material-Menu"
    , Page.mds "https://www.google.com/design/spec/components/menus.html"
    , Page.mdl "https://www.getmdl.io/components/#menus-section"
    ]
