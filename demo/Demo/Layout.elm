module Demo.Layout exposing (..)

import Platform.Cmd exposing (Cmd, none)
import Html exposing (..)
import Html.Events
import String
import Array exposing (Array)

import Material.Toggles as Toggles
import Material.Options as Options exposing (css, cs, when)
import Material 
import Material.Grid as Grid
import Material.Color as Color
import Material.Elevation as Elevation
import Material.Typography as Typography

import Demo.Page as Page
import Demo.Code as Code


-- MODEL


type alias Mdl = 
  Material.Model 


type HeaderType 
  = Waterfall Bool
  | Seamed
  | Standard
  | Scrolling


type alias Model =
  { mdl : Material.Model
  , fixedHeader : Bool
  , fixedDrawer : Bool
  , fixedTabs : Bool
  , header : HeaderType
  , rippleTabs : Bool
  , transparentHeader : Bool
  , withDrawer : Bool
  , withHeader : Bool
  , withTabs : Bool
  , primary : Color.Hue
  , accent : Color.Hue
  }


model : Model
model =
  { mdl = Material.model
  , fixedHeader = True
  , fixedTabs = False
  , fixedDrawer = False
  , header = Standard
  , rippleTabs = True
  , transparentHeader = False
  , withDrawer = True
  , withHeader = True
  , withTabs = True
  , primary = Color.Teal
  , accent = Color.Red
  }


-- ACTION, UPDATE


type Msg 
  = TemplateMsg 
  | Update (Model -> Model)
  | Mdl (Material.Msg Msg)


update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    TemplateMsg -> 
      (model, Cmd.none)

    Update f -> 
      (f model, Cmd.none)

    Mdl action' -> 
      Material.update action' model


{- Make sure we didn't pick the same primary and accent colour. -}
fixColors : Model -> Model
fixColors model = 
  if model.primary == model.accent then 
    if model.primary == Color.Indigo then 
      { model | accent = Color.Red }
    else 
      { model | accent = Color.Indigo }
  else
    model





-- VIEW


table : List (Html m) -> Grid.Cell m
table contents = 
  Grid.cell 
    [] 
    [ Options.div 
        [ css "display" "inline-flex"
        , css "flex-direction" "column"
        , css "width" "auto"
        ]
        contents
    ]
    

explain : String -> Html m
explain str = 
  Options.styled p [ css "margin-top" "1ex", css "margin-bot" "2ex" ] [ text str ]


picker : 
  Array (Color.Hue) -> 
  Maybe Color.Hue -> 
  Color.Shade -> 
  Color.Hue -> (Color.Hue -> Model -> Model) -> Html Msg
picker hues disabled shade current f = 
  hues
    |> Array.toList
    |> List.map (\hue -> 
      Options.styled' div 
        [ Color.background (Color.color hue shade) `when` (disabled /= Just hue)
        , Color.background (Color.color Color.Grey Color.S200) `when` (disabled == Just hue)
        , css "width" "56px"
        , css "height" "56px"
        , css "margin" "2pt"
        , css "line-height" "56px"
        , css "flex-shrink" "0"
        , Elevation.e8 `when` (current == hue)
        , css "transition" "box-shadow 300ms ease-in-out 0s, background-color 300ms ease-in-out 0s"
        , css "cursor" "pointer" `when` (disabled /= Just hue)
        ] 
        (if Just hue /= disabled then 
          [ Html.Events.onClick (f hue >> fixColors |> Update) ]
         else
          [])
        [
        ]
      )
    |> Options.div 
         [ css "display" "flex"
         , css "flex-wrap" "wrap"
         ] 


heading : Color.Hue -> Html Msg
heading current = 
  Options.div 
    [ css "align-self" "flex-end" 
    , Typography.display3
    , Color.text (Color.color current Color.S500)
    ] 
    [ text (Color.hueName current) ] 



view : Model -> Html Msg
view model  =
  let
    demo1 = 
      [ Grid.grid
          []
          [ table 
              [ h4 [] [ text "Included sub-components" ]
              , Toggles.switch Mdl [8] model.mdl
                  [ Toggles.onClick (Update <| \m -> { m | withHeader = not m.withHeader })
                  , Toggles.value model.withHeader
                  ]
                  [ text "With header" ]
              , Toggles.switch Mdl [9] model.mdl
                  [ Toggles.onClick (Update <| \m -> { m | withDrawer = not m.withDrawer })
                  , Toggles.value model.withDrawer
                  ]
                  [ text "With drawer" ]
              , Toggles.switch Mdl [10] model.mdl
                  [ Toggles.onClick (Update <| \m -> { m | withTabs = not m.withTabs })
                  , Toggles.value model.withTabs
                  ]
                  [ text "With tabs" ]
              ]
          , table
              [ h4 [] [ text "Size-dependent behaviour" ]
              , Toggles.switch Mdl [0] model.mdl
                  [ Toggles.onClick (Update <| \m -> { m | fixedHeader = not m.fixedHeader })
                  , Toggles.value model.fixedHeader
                  ]
                  [ text "Fixed header" ]
              , explain """The header by default disappears on small devices. 
                           This option forces the display of the header on all devices. """ 
              , Toggles.switch Mdl [1] model.mdl 
                  [ Toggles.onClick (Update <| \m -> { m | fixedDrawer = not m.fixedDrawer })
                  , Toggles.value model.fixedDrawer
                  ]
                  [ text "Fixed drawer" ]
              , explain """The drawer is by default hidden on all devices. 
                           This option forces the drawer to be open on large devices. """ 
              , Toggles.switch Mdl [2] model.mdl
                  [ Toggles.onClick (Update <| \m -> { m | fixedTabs = not m.fixedTabs })
                  , Toggles.value model.fixedTabs
                  ]
                  [ text "Fixed tabs" ]
              , explain """The tabs by default extend from left to right, scrolling 
                           when necessary. This option forces tabs to spread out to 
                           consume all available space.""" 
              ]
          , table 
              [ h4 [] [ text "Header behaviour" ] 
              , Toggles.radio Mdl [3] model.mdl
                  [ Toggles.group "kind" 
                  , Toggles.value <| model.header == Standard 
                  , Toggles.onClick (Update <| \m -> { m | header = Standard })
                  ]
                  [ text "Standard" ]
              , Toggles.radio Mdl [4] model.mdl
                  [ Toggles.group "kind" 
                  , Toggles.value <| model.header == Seamed 
                  , Toggles.onClick (Update <| \m -> { m | header = Seamed })
                  ]
                  [ text "Seamed" ]
              , Toggles.radio Mdl [5] model.mdl
                  [ Toggles.group "kind" 
                  , Toggles.value <| model.header == Scrolling 
                  , Toggles.onClick (Update <| \m -> { m | header = Scrolling })
                  ]
                  [ text "Scrolling" ]
              , Toggles.radio Mdl [6] model.mdl
                  [ Toggles.group "kind" 
                  , Toggles.value <| model.header == (Waterfall True) 
                  , Toggles.onClick (Update <| \m -> { m | header = (Waterfall True) })
                  ]
                  [ text "Waterfall (top)" ]
              , Toggles.radio Mdl [7] model.mdl
                  [ Toggles.group "kind" 
                  , Toggles.value <| model.header == (Waterfall False) 
                  , Toggles.onClick (Update <| \m -> { m | header = (Waterfall False) })
                  ]
                  [ text "Waterfall (bottom)" ]
              ]
          ]
      , div [] 
          [ let 
              options = 
                [ if model.fixedHeader then Just "Layout.fixedHeader" else Nothing
                , if model.fixedDrawer then Just "Layout.fixedDrawer" else Nothing
                , if model.fixedTabs then Just "Layout.fixedTabs" else Nothing
                , if model.withHeader then
                      case model.header of 
                        Waterfall x -> Just <| "Layout.waterfall " ++ toString x
                        Seamed -> Just <| "Layout.seamed"
                        Scrolling -> Just <| "Layout.scrolling"
                        Standard -> Nothing
                  else
                    Nothing
                ] 
              body = 
                "  { header = " ++ (if model.withHeader then "[ ... ]" else "[]") ++ "\n" ++
                "  , drawer = " ++ (if model.withDrawer then "[ ... ]" else "[]") ++ "\n" ++
                "  , tabs = "   ++ (if model.withTabs   then "[ ... ]" else "[]") ++ "\n" ++
                "  , main = [ ... ]\n" ++ 
                "  }"
            in
              options 
              |> List.filterMap identity
              |> String.join "\n  , "
              |> (++) "import Material.Layout as Layout\n\nLayout.render Mdl model.mdl\n  [ " 
              |> flip (++) "\n  ]\n"
              |> flip (++) body
              |> Code.code []
          ]
      ]
    demo2 = 
      [ div [] 
          [ h4 [] [ text "Colour" ] 
          , explain """While technically not part of the layout Component, 
                       this is a convenient place to demonstrate colour styling.
                       Change the scheme by clicking below. """
          , explain """Changing the colour scheme affects not just Layout, but
                       most components. Try changing the scheme, then look what
                       happens to Buttons, Badges, or Textfields. """
          , Grid.grid [ Grid.noSpacing ] 
              [ Grid.cell 
                  [ Grid.size Grid.All 4 ]
                  [ h5 [] [ text "Primary colour" ] 
                  , picker Color.hues Nothing Color.S500 model.primary (\hue m -> { m | primary = hue }) 
                  ]
              , Grid.cell 
                  [ Grid.size Grid.All 4 
                  , Grid.offset Grid.Desktop 2 
                  ]
                  [ h5 [] [ text "Accent colour" ] 
                  , picker Color.accentHues (Just model.primary) Color.A200 model.accent (\hue m -> { m | accent = hue })
                  ]
              , Grid.cell
                  [ Grid.size Grid.All 4
                  , css "text-align" "right"
                  , Grid.order Grid.Phone 0
                  ]
                  [ heading model.primary ]
              , Grid.cell 
                  [ Grid.size Grid.All 4, Grid.offset Grid.Desktop 2 
                  , css "text-align" "right"
                  ]
                  [ heading model.accent
                  ]
              ]
          , Options.div [ css "height" "3rem" ] [] 
          , explain "To use this colour scheme (and to use elm-mdl in general), you must load custom CSS."
          , [ "<link href='https://fonts.googleapis.com/css?family=Roboto:400,300,500|Roboto+Mono|Roboto+Condensed:400,700&subset=latin,latin-ext' rel='stylesheet' type='text/css'>"
            , "<link rel='stylesheet' href='https://fonts.googleapis.com/icon?family=Material+Icons'>"
            , "<link rel='stylesheet' href='https://code.getmdl.io/1.1.3/" ++ Color.scheme model.primary model.accent 
                  ++ "'>"
            ]
            |> String.join "\n"
            |> Code.html []
          , explain """For quick experiments, you can alternatively load CSS directly from Elm as follows.
                       NB! This approach will cause flickering on load and so is
                       not suitable anything but quick experiments."""
          , Code.code [] <|
              """import Material.Scheme as Scheme
    import Material.Color as Color

    -- Wrap your main html, say, "contents" like this: 
    myView = 
      Scheme.topWithScheme """ ++ toString model.primary ++ 
         " " ++ toString model.accent ++ " contents"
          ]
      ]
  in
    Page.body1' "Layout" srcUrl intro references demo1 demo2


intro : Html m
intro = 
  Page.fromMDL "https://getmdl.io/components/index.html#layout-section/layout" """
> The Material Design Lite (MDL) layout component is a comprehensive approach to
> page layout that uses MDL development tenets, allows for efficient use of MDL
> components, and automatically adapts to different browsers, screen sizes, and
> devices.
> 
> Appropriate and accessible layout is a critical feature of all user interfaces,
> regardless of a site's content or function. Page design and presentation is
> therefore an important factor in the overall user experience. See the layout
> component's Material Design specifications page for details.
> 
> Use of MDL layout principles simplifies the creation of scalable pages by
> providing reusable components and encourages consistency across environments by
> establishing recognizable visual elements, adhering to logical structural
> grids, and maintaining appropriate spacing across multiple platforms and screen
> sizes. MDL layout is extremely powerful and dynamic, allowing for great
> consistency in outward appearance and behavior while maintaining development
> flexibility and ease of use.
""" 


srcUrl : String 
srcUrl =
  "https://github.com/debois/elm-mdl/blob/master/demo/Demo/Layout.elm"


references : List (String, String)
references = 
  [ Page.package "http://package.elm-lang.org/packages/debois/elm-mdl/latest/Material-Layout"
  , Page.mds "http://www.google.com/design/spec/layout/principles.html"
  , Page.mdl "https://www.getmdl.io/components/index.html#layout-section/layout"
  ]


