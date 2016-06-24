
import Html exposing (..)
import Html.Attributes exposing (href, class, style)
import Html.Lazy
import Html.App as App
import Platform.Cmd exposing (..)
import Array exposing (Array)
import Dict exposing (Dict)
import String

import Navigation
import RouteUrl as Routing

import Material
import Material.Color as Color
import Material.Layout as Layout 
import Material.Helpers exposing (pure, lift, lift')
import Material.Options as Options exposing (css, when)
import Material.Scheme as Scheme
import Material.Icon as Icon

import Demo.Buttons
import Demo.Menus
import Demo.Tables
import Demo.Grid
import Demo.Textfields
import Demo.Snackbar
import Demo.Badges
import Demo.Elevation
import Demo.Toggles
import Demo.Loading
import Demo.Layout
import Demo.Footer
import Demo.Tooltip
import Demo.Tabs
--import Demo.Template


-- MODEL



type alias Model =
  { mdl : Material.Model
  , buttons : Demo.Buttons.Model
  , badges : Demo.Badges.Model
  , layout : Demo.Layout.Model
  , menus : Demo.Menus.Model
  , textfields : Demo.Textfields.Model
  , toggles : Demo.Toggles.Model
  , snackbar : Demo.Snackbar.Model
  , tables : Demo.Tables.Model
  , loading : Demo.Loading.Model
  , footers : Demo.Footer.Model
  , tooltip : Demo.Tooltip.Model
  , tabs : Demo.Tabs.Model
  --, template : Demo.Template.Model
  , selectedTab : Int
  , transparentHeader : Bool
  }


model : Model
model =
  { mdl = Material.model
  , buttons = Demo.Buttons.model
  , badges = Demo.Badges.model
  , layout = Demo.Layout.model
  , menus = Demo.Menus.model
  , textfields = Demo.Textfields.model
  , toggles = Demo.Toggles.model
  , snackbar = Demo.Snackbar.model
  , tables = Demo.Tables.model
  , loading = Demo.Loading.model
  , footers = Demo.Footer.model
  , tooltip = Demo.Tooltip.model
  , tabs = Demo.Tabs.model
  --, template = Demo.Template.model
  , selectedTab = 0
  , transparentHeader = False
  }



-- ACTION, UPDATE


type Msg
  = SelectTab Int
  | Mdl Material.Msg
  | BadgesMsg Demo.Badges.Msg
  | ButtonsMsg Demo.Buttons.Msg
  | LayoutMsg Demo.Layout.Msg
  | MenusMsg Demo.Menus.Msg
  | TextfieldMsg Demo.Textfields.Msg
  | SnackbarMsg Demo.Snackbar.Msg
  | TogglesMsg Demo.Toggles.Msg
  | TablesMsg Demo.Tables.Msg
  | LoadingMsg Demo.Loading.Msg
  | FooterMsg Demo.Footer.Msg
  | TooltipMsg Demo.Tooltip.Msg
  | TabMsg Demo.Tabs.Msg
  | ToggleHeader
  --| TemplateMsg Demo.Template.Msg


nth : Int -> List a -> Maybe a
nth k xs = 
  List.drop k xs |> List.head


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
  case action of
    SelectTab k -> 
      ( { model | selectedTab = k } , Cmd.none )

    ToggleHeader ->
      ( { model | transparentHeader = not model.transparentHeader }, Cmd.none)

    Mdl msg -> 
      Material.update Mdl msg model

    ButtonsMsg   a -> lift  .buttons    (\m x->{m|buttons   =x}) ButtonsMsg  Demo.Buttons.update    a model

    BadgesMsg    a -> lift  .badges     (\m x->{m|badges    =x}) BadgesMsg   Demo.Badges.update    a model

    LayoutMsg a -> lift  .layout    (\m x->{m|layout   =x}) LayoutMsg  Demo.Layout.update    a model

    MenusMsg a -> lift  .menus    (\m x->{m|menus   =x}) MenusMsg  Demo.Menus.update    a model

    TextfieldMsg a -> lift  .textfields (\m x->{m|textfields=x}) TextfieldMsg Demo.Textfields.update a model

    SnackbarMsg  a -> lift  .snackbar   (\m x->{m|snackbar  =x}) SnackbarMsg Demo.Snackbar.update   a model

    TogglesMsg    a -> lift .toggles   (\m x->{m|toggles    =x}) TogglesMsg Demo.Toggles.update   a model

    TablesMsg   a -> lift  .tables    (\m x->{m|tables   =x}) TablesMsg  Demo.Tables.update    a model
--
    LoadingMsg   a -> lift  .loading    (\m x->{m|loading   =x}) LoadingMsg  Demo.Loading.update    a model
    FooterMsg   a -> lift  .footers    (\m x->{m|footers   =x}) FooterMsg  Demo.Footer.update    a model

    TooltipMsg   a -> lift  .tooltip    (\m x->{m|tooltip   =x}) TooltipMsg  Demo.Tooltip.update    a model
    TabMsg   a -> lift  .tabs    (\m x->{m|tabs   =x}) TabMsg  Demo.Tabs.update    a model


    --TemplateMsg  a -> lift  .template   (\m x->{m|template  =x}) TemplateMsg Demo.Template.update   a model


-- VIEW


tabs : List (String, String, Model -> Html Msg)
tabs =
  [ ("Buttons", "buttons", .buttons >> Demo.Buttons.view >> App.map ButtonsMsg)
  , ("Badges", "badges", .badges >> Demo.Badges.view >> App.map BadgesMsg)
  , ("Elevation", "elevation", \_ -> Demo.Elevation.view)
  , ("Footers", "footers", .footers >> Demo.Footer.view >> App.map FooterMsg)
  , ("Grid", "grid", \_ -> Demo.Grid.view)
  , ("Layout", "layout", .layout >> Demo.Layout.view >> App.map LayoutMsg)
  , ("Loading", "loading", .loading >> Demo.Loading.view >> App.map LoadingMsg)
  , ("Menus", "menus", .menus >> Demo.Menus.view >> App.map MenusMsg)
  , ("Snackbar", "snackbar", .snackbar >> Demo.Snackbar.view >> App.map SnackbarMsg)
  , ("Textfields", "textfields", .textfields >> Demo.Textfields.view >> App.map TextfieldMsg)
  , ("Toggles", "toggles", .toggles >> Demo.Toggles.view >> App.map TogglesMsg)
  , ("Tables", "tables", .tables >> Demo.Tables.view >> App.map TablesMsg)
  , ("Tooltips", "tooltips", .tooltip >> Demo.Tooltip.view >> App.map TooltipMsg)
  , ("Tabs", "tabs", .tabs >> Demo.Tabs.view >> App.map TabMsg)
  --, ("Template", "template", .template >> Demo.Template.view >> App.map TemplateMsg)
  ]


tabTitles : List (Html a)
tabTitles =
  List.map (\(x,_,_) -> text x) tabs


tabViews : Array (Model -> Html Msg)
tabViews = List.map (\(_,_,v) -> v) tabs |> Array.fromList


tabUrls : Array String
tabUrls = 
  List.map (\(_,x,_) -> x) tabs |> Array.fromList


urlTabs : Dict String Int
urlTabs = 
  List.indexedMap (\idx (_,x,_) -> (x, idx)) tabs |> Dict.fromList



e404 : Model -> Html Msg
e404 _ =  
  div 
    [ 
    ]
    [ Options.styled Html.h1
        [ Options.cs "mdl-typography--display-4" 
        , Color.background Color.primary 
        ]
        [ text "404" ]
    ]



drawer : List (Html Msg)
drawer =
  [ Layout.title [] [ text "Example drawer" ]
  , Layout.navigation
    [] 
    [  Layout.link
        [ Layout.href "https://github.com/debois/elm-mdl" ]
        [ text "github" ]
    , Layout.link
        [ Layout.href "http://package.elm-lang.org/packages/debois/elm-mdl/latest/" ]
        [ text "elm-package" ]
    ]
  ]


header : Model -> List (Html Msg)
header model =
  if model.layout.withHeader then 
    [ Layout.row 
        [ if model.transparentHeader then css "height" "192px" else Options.nop 
        , css "transition" "height 333ms ease-in-out 0s"
        ]
        [ Layout.title [] [ text "elm-mdl" ]
        , Layout.spacer
        , Layout.navigation []
            [ Layout.link
                [ Layout.onClick ToggleHeader]
                [ Icon.i "photo" ]
            , Layout.link
                [ Layout.href "https://github.com/debois/elm-mdl"]
                [ span [] [text "github"] ]
            , Layout.link
                [ Layout.href "http://package.elm-lang.org/packages/debois/elm-mdl/latest/" ]
                [ text "elm-package" ]
            ]
        ]
    ]
  else
    []


view : Model -> Html Msg
view = Html.Lazy.lazy view'


view' : Model -> Html Msg
view' model =
  let
    top =
      (Array.get model.selectedTab tabViews |> Maybe.withDefault e404) model
      
  in
    Layout.render Mdl model.mdl
      [ Layout.selectedTab model.selectedTab
      , Layout.onSelectTab SelectTab
      , Layout.fixedHeader `when` model.layout.fixedHeader 
      , Layout.fixedDrawer `when` model.layout.fixedDrawer
      , Layout.fixedTabs `when` model.layout.fixedTabs
      , (case model.layout.header of
          Demo.Layout.Waterfall x -> Layout.waterfall x
          Demo.Layout.Seamed -> Layout.seamed
          Demo.Layout.Standard -> Options.nop
          Demo.Layout.Scrolling -> Layout.scrolling)
        `when` model.layout.withHeader
      , if model.transparentHeader then Layout.transparentHeader else Options.nop
      ]
      { header = header model
      , drawer = if model.layout.withDrawer then drawer else []
      , tabs = 
          if model.layout.withTabs then 
            (tabTitles, [ Color.background (Color.color model.layout.primary Color.S400) ])
          else
            ([], [])
      , main = [ stylesheet, top ]
      }
    {- ** The following lines are not necessary when you manually set up
       your html, as done with page.html. Removing it will then
       fix the flicker you see on load.
    -}
    |> (\contents -> 
      div []
        [ Scheme.topWithScheme model.layout.primary model.layout.accent contents
        , Html.node "script"
           [ Html.Attributes.attribute "src" "assets/highlight.pack.js" ]
           []
        ]
    )


-- ROUTING


urlOf : Model -> String
urlOf model = 
  "#" ++ (Array.get model.selectedTab tabUrls |> Maybe.withDefault "")


delta2url : Model -> Model -> Maybe Routing.UrlChange
delta2url model1 model2 = 
  if model1.selectedTab /= model2.selectedTab then 
    { entry = Routing.NewEntry
    , url = urlOf model2
    } |> Just
  else
    Nothing
        

location2messages : Navigation.Location -> List Msg
location2messages location = 
  [ case String.dropLeft 1 location.hash of
      "" -> 
        SelectTab 0
  
      x -> 
        Dict.get x urlTabs
          |> Maybe.withDefault -1
          |> SelectTab
  ]
  


-- APP


main : Program Never
main =
  Routing.program 
    { delta2url = delta2url
    , location2messages = location2messages
    , init = 
        ( { model
          | mdl = Layout.setTabsWidth 1384 model.mdl
          }
          , Layout.sub0 Mdl 
        )
    , view = view
    , subscriptions = .mdl >> Layout.subs Mdl
    , update = update
    }


-- CSS


stylesheet : Html a
stylesheet =
  Options.stylesheet """
  /* The following line is better done in html. We keep it here for
     compatibility with elm-reactor.
   */
  @import url("assets/styles/github-gist.css");

  blockquote:before { content: none; }
  blockquote:after { content: none; }
  blockquote {
    border-left-style: solid;
    border-width: 1px;
    padding-left: 1.3ex;
    border-color: rgb(255,82,82);
    font-style: normal;
      /* TODO: Really need a way to specify "secondary color" in
         inline css.
       */
  }
  p, blockquote { 
    max-width: 40em;
  }

  h1, h2 { 
    /* TODO. Need typography module with kerning. */
    margin-left: -3px;
  }

  pre { 
    display: inline-block; 
    box-sizing: border-box;
    min-width: 100%;
    background-color: #f8f8f8; 
    padding-top: .5rem;
    padding-bottom: 1rem;
    padding-left:1rem;
    margin: 0;
  }
  .mdl-layout__header--transparent {
    background: url('https://getmdl.io/assets/demos/transparent.jpg') center / cover;
  }
  .mdl-layout__header--transparent .mdl-layout__drawer-button {
    /* This background is dark, so we set text to white. Use 87% black instead if
       your background is light. */
    color: white;
  }
"""
