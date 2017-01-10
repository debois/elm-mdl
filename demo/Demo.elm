module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href, class, style)
import Html.Lazy
import Platform.Cmd exposing (..)
import Array exposing (Array)
import Dict exposing (Dict)
import String
import Navigation
import RouteUrl as Routing
import Material
import Material.Color as Color
import Material.Layout as Layout
import Material.Helpers exposing (pure, lift, map1st, map2nd)
import Material.Options as Options exposing (css, when)
import Material.Scheme as Scheme
import Material.Icon as Icon
import Material.Typography as Typography
import Material.Menu as Menu
import Material.Toggles as Toggles
import Material.Select as Select

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
import Demo.Slider
import Demo.Typography
import Demo.Cards
import Demo.Lists
import Demo.Dialog
import Demo.Chips
import Demo.Select


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
    , slider : Demo.Slider.Model
    , typography : Demo.Typography.Model
    , cards : Demo.Cards.Model
    , lists : Demo.Lists.Model
    , dialog : Demo.Dialog.Model
    , elevation : Demo.Elevation.Model
    , chips : Demo.Chips.Model
    , select : Demo.Select.Model
    , selectedTab : Int
    , transparentHeader : Bool
    , logMessages : Bool
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
    , slider = Demo.Slider.model
    , typography = Demo.Typography.model
    , cards = Demo.Cards.model
    , lists = Demo.Lists.model
    , dialog = Demo.Dialog.model
    , elevation = Demo.Elevation.model
    , chips = Demo.Chips.model
    , select = Demo.Select.model
    , selectedTab = 0
    , transparentHeader = False
    , logMessages = False
    }



-- ACTION, UPDATE


type Msg
    = SelectTab Int
    | Mdl (Material.Msg Msg)
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
    | SliderMsg Demo.Slider.Msg
    | TypographyMsg Demo.Typography.Msg
    | CardsMsg Demo.Cards.Msg
    | ListsMsg Demo.Lists.Msg
    | DialogMsg Demo.Dialog.Msg
    | ElevationMsg Demo.Elevation.Msg
    | ChipMsg Demo.Chips.Msg
    | SelectMsg Demo.Select.Msg
    | ToggleHeader
    | ToggleLog


nth : Int -> List a -> Maybe a
nth k xs =
    List.drop k xs |> List.head


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let 
        log msg = 
            if model.logMessages then Debug.log "Msg" else identity
    in
      case log "Msg" msg of
          SelectTab k ->
              ( { model | selectedTab = k }, Cmd.none )

          ToggleHeader ->
              ( { model | transparentHeader = not model.transparentHeader }, Cmd.none )
          ToggleLog -> 
              ( { model | logMessages = not model.logMessages }, Cmd.none )

          Mdl msg ->
              Material.update Mdl msg model

          ButtonsMsg a ->
              lift .buttons (\m x -> { m | buttons = x }) ButtonsMsg Demo.Buttons.update a model

          BadgesMsg a ->
              lift .badges (\m x -> { m | badges = x }) BadgesMsg Demo.Badges.update a model

          LayoutMsg a ->
              lift .layout (\m x -> { m | layout = x }) LayoutMsg Demo.Layout.update a model

          MenusMsg a ->
              lift .menus (\m x -> { m | menus = x }) MenusMsg Demo.Menus.update a model

          TextfieldMsg m ->
              Demo.Textfields.update m model.textfields
                  |> Maybe.map (map1st (\x -> { model | textfields = x }))
                  |> Maybe.withDefault ( model, Cmd.none )
                  |> map2nd (Cmd.map TextfieldMsg)

          SnackbarMsg a ->
              lift .snackbar (\m x -> { m | snackbar = x }) SnackbarMsg Demo.Snackbar.update a model

          TogglesMsg a ->
              lift .toggles (\m x -> { m | toggles = x }) TogglesMsg Demo.Toggles.update a model

          TablesMsg a ->
              lift .tables (\m x -> { m | tables = x }) TablesMsg Demo.Tables.update a model

          LoadingMsg a ->
              lift .loading (\m x -> { m | loading = x }) LoadingMsg Demo.Loading.update a model

          FooterMsg a ->
              lift .footers (\m x -> { m | footers = x }) FooterMsg Demo.Footer.update a model

          SliderMsg a ->
              lift .slider (\m x -> { m | slider = x }) SliderMsg Demo.Slider.update a model

          TooltipMsg a ->
              lift .tooltip (\m x -> { m | tooltip = x }) TooltipMsg Demo.Tooltip.update a model

          TabMsg a ->
              lift .tabs (\m x -> { m | tabs = x }) TabMsg Demo.Tabs.update a model

          TypographyMsg a ->
              lift .typography (\m x -> { m | typography = x }) TypographyMsg Demo.Typography.update a model

          CardsMsg a ->
              lift .cards (\m x -> { m | cards = x }) CardsMsg Demo.Cards.update a model

          ListsMsg a ->
              lift .lists (\m x -> { m | lists = x }) ListsMsg Demo.Lists.update a model

          DialogMsg a ->
              lift .dialog (\m x -> { m | dialog = x }) DialogMsg Demo.Dialog.update a model

          ElevationMsg a ->
              lift .elevation (\m x -> { m | elevation = x }) ElevationMsg Demo.Elevation.update a model

          ChipMsg a ->
              lift .chips (\m x -> { m | chips = x }) ChipMsg Demo.Chips.update a model
          SelectMsg a ->
              lift .select (\m x -> { m | select = x }) SelectMsg Demo.Select.update a model



-- VIEW


tabs : List ( String, String, Model -> Html Msg )
tabs =
    [ ( "Buttons", "buttons", .buttons >> Demo.Buttons.view >> Html.map ButtonsMsg )
    , ( "Select", "select", .select >> Demo.Select.view >> Html.map SelectMsg)
    , ( "Menus", "menus", .menus >> Demo.Menus.view >> Html.map MenusMsg )
    , ( "Badges", "badges", .badges >> Demo.Badges.view >> Html.map BadgesMsg )
    , ( "Cards", "cards", .cards >> Demo.Cards.view >> Html.map CardsMsg )
    , ( "Chips", "chips", .chips >> Demo.Chips.view >> Html.map ChipMsg )
    , ( "Dialog", "dialog", .dialog >> Demo.Dialog.view >> Html.map DialogMsg )
    , ( "Elevation", "elevation", .elevation >> Demo.Elevation.view >> Html.map ElevationMsg )
    , ( "Footers", "footers", .footers >> Demo.Footer.view >> Html.map FooterMsg )
    , ( "Grid", "grid", \_ -> Demo.Grid.view )
    , ( "Layout", "layout", .layout >> Demo.Layout.view >> Html.map LayoutMsg )
    , ( "Lists", "lists", .lists >> Demo.Lists.view >> Html.map ListsMsg )
    , ( "Loading", "loading", .loading >> Demo.Loading.view >> Html.map LoadingMsg )
    , ( "Sliders", "sliders", .slider >> Demo.Slider.view >> Html.map SliderMsg )
    , ( "Snackbar", "snackbar", .snackbar >> Demo.Snackbar.view >> Html.map SnackbarMsg )
    , ( "Tables", "tables", .tables >> Demo.Tables.view >> Html.map TablesMsg )
    , ( "Tabs", "tabs", .tabs >> Demo.Tabs.view >> Html.map TabMsg )
    , ( "Textfields", "textfields", .textfields >> Demo.Textfields.view >> Html.map TextfieldMsg )
    , ( "Toggles", "toggles", .toggles >> Demo.Toggles.view >> Html.map TogglesMsg )
    , ( "Tooltips", "tooltips", .tooltip >> Demo.Tooltip.view >> Html.map TooltipMsg )
    , ( "Typography", "typography", .typography >> Demo.Typography.view >> Html.map TypographyMsg )
    ]


tabTitles : List (Html a)
tabTitles =
    List.map (\( x, _, _ ) -> text x) tabs


tabViews : Array (Model -> Html Msg)
tabViews =
    List.map (\( _, _, v ) -> v) tabs |> Array.fromList


tabUrls : Array String
tabUrls =
    List.map (\( _, x, _ ) -> x) tabs |> Array.fromList


urlTabs : Dict String Int
urlTabs =
    List.indexedMap (\idx ( _, x, _ ) -> ( x, idx )) tabs |> Dict.fromList


e404 : Model -> Html Msg
e404 _ =
    div
        []
        [ Options.styled Html.h1
            [ Options.cs "mdl-typography--display-4"
            , Typography.center
            ]
            [ text "404" ]
        ]


drawer : Model -> List (Html Msg)
drawer model =
    [ Layout.title [] [ text "elm-mdl" ]
    , Layout.navigation
        []
        [ Layout.link
            [ Layout.href "https://github.com/debois/elm-mdl" ]
            [ text "github" ]
        , Layout.link
            [ Layout.href "http://package.elm-lang.org/packages/debois/elm-mdl/latest/" ]
            [ text "elm-package" ]
        , Layout.link
            [ Layout.href "#cards"
            , Options.onClick (Layout.toggleDrawer Mdl)
            ]
            [ text "Card component" ]
        , Layout.link
            [ css "display" "inline-flex" 
            , css "flex-wrap" "wrap"
            , css "justify-content" "space-between" 
            , css "align-items" "center" 
            , Options.onToggle ToggleLog
            , Options.onClick ToggleLog
            ]
            [ text "Log messages" 
            , Toggles.checkbox Mdl [0] model.mdl
                [ Toggles.ripple
                , Toggles.value model.logMessages
                , css "width" "32px"
                ]
                []
            , if model.logMessages then 
                Options.div 
                  [ Typography.caption 
                  , css "width" "100%"
                  ]
                  [ text "Open your Javascript console to observe MDL messages" ] 
                
              else
                text ""
            ]
        ]
    ]


header : Model -> List (Html Msg)
header model =
    if model.layout.withHeader then
        [ Layout.row
            [ if model.transparentHeader then
                css "height" "192px"
              else
                Options.nop
            , css "transition" "height 333ms ease-in-out 0s"
            ]
            [ Layout.title [] [ text "elm-mdl" ]
            , Layout.spacer
            , Layout.navigation []
                [ Layout.link
                    [ Options.onClick ToggleHeader ]
                    [ Icon.i "photo" ]
                , Layout.link
                    [ Layout.href "https://github.com/debois/elm-mdl" ]
                    [ span [] [ text "github" ] ]
                , Layout.link
                    [ Layout.href "http://package.elm-lang.org/packages/debois/elm-mdl/latest/" ]
                    [ text "elm-package" ]
                ]
            ]
        ]
    else
        []


view : Model -> Html Msg
view =
    Html.Lazy.lazy view_


view_ : Model -> Html Msg
view_ model =
    let
        top =
            (Array.get model.selectedTab tabViews |> Maybe.withDefault e404) model
    in
        Layout.render Mdl
            model.mdl
            [ Layout.selectedTab model.selectedTab
            , Layout.onSelectTab SelectTab
            , Layout.fixedHeader |> when model.layout.fixedHeader
            , Layout.fixedDrawer |> when model.layout.fixedDrawer
            , Layout.fixedTabs |> when model.layout.fixedTabs
            , (case model.layout.header of
                Demo.Layout.Waterfall x ->
                    Layout.waterfall x

                Demo.Layout.Seamed ->
                    Layout.seamed

                Demo.Layout.Standard ->
                    Options.nop

                Demo.Layout.Scrolling ->
                    Layout.scrolling
              )
                |> when model.layout.withHeader
            , if model.transparentHeader then
                Layout.transparentHeader
              else
                Options.nop
            ]
            { header = header model
            , drawer =
                if model.layout.withDrawer then
                    drawer model
                else
                    []
            , tabs =
                if model.layout.withTabs then
                    ( tabTitles, [ Color.background (Color.color model.layout.primary Color.S400) ] )
                else
                    ( [], [] )
            , main = [ stylesheet, top ]
            }
            {- ** Begin

               The following lines are not necessary when you manually set up
               your html, as done with page.html. Removing it will then
               fix the flicker you see on load.
            -}
            |>
                (\contents ->
                    div []
                        [ Scheme.topWithScheme model.layout.primary model.layout.accent contents
                        , Html.node "script"
                            [ Html.Attributes.attribute "src" "https://cdn.polyfill.io/v2/polyfill.js?features=Event.focusin" ]
                            []
                        , Html.node "script"
                            [ Html.Attributes.attribute "src" "assets/highlight/highlight.pack.js" ]
                            []
                        , case nth model.selectedTab tabs of
                            Just ( "Dialog", _, _ ) ->
                                Html.map DialogMsg (Demo.Dialog.element model.dialog)

                            {- Because of limitations on browsers that have non-native (polyfilled)
                               <dialog> elements, our dialog element /may/ have to sit up here. However,
                               running in elm-reactor will never load the polyfill, so we render the
                               dialog (wrongly if there is no polyfill) only when the Dialog tab is
                               active.
                            -}
                            _ ->
                                div [] []
                        ]
                )



{- ** End -}
-- ROUTING


urlOf : Model -> String
urlOf model =
    "#" ++ (Array.get model.selectedTab tabUrls |> Maybe.withDefault "")


delta2url : Model -> Model -> Maybe Routing.UrlChange
delta2url model1 model2 =
    if model1.selectedTab /= model2.selectedTab then
        { entry = Routing.NewEntry
        , url = urlOf model2
        }
            |> Just
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


main : Routing.RouteUrlProgram Never Model Msg
main =
    Routing.program
        { delta2url = delta2url
        , location2messages = location2messages
        , init =
            ( { model
                | mdl =
                    Layout.setTabsWidth 2124 model.mdl
                    {- elm gives us no way to measure the actual width of tabs. We
                       hardwire it. If you add a tab, remember to update this. Find the
                       new value using:

                       document.getElementsByClassName("mdl-layout__tab-bar")[0].scrollWidth
                    -}
              }
            , Material.init Mdl
            )
        , view = view
        , subscriptions =
            \model ->
                Sub.batch
                    [ Sub.map MenusMsg (Menu.subs Demo.Menus.Mdl model.menus.mdl)
                    , Sub.map SelectMsg (Select.subs Demo.Select.Mdl model.select.mdl)
                    , Material.subscriptions Mdl model
                    ]
        , update = update
        }


-- CSS


stylesheet : Html a
stylesheet =
    Options.stylesheet """
  /* The following line is better done in html. We keep it here for
     compatibility with elm-reactor.
   */
  @import url("assets/highlight/github-gist.css");

  blockquote:before { content: none; }
  blockquote:after { content: none; }
  blockquote {
    border-left-style: solid;
    border-width: 1px;
    padding-left: 1.3ex;
    border-color: rgb(255,82,82);
      /* Really need a way to specify "secondary color" in
         inline css.
       */
    font-style: normal;
  }
  p, blockquote {
    max-width: 40em;
  }

  pre {
    display: inline-block;
    box-sizing: border-box;
    min-width: 100%;
    padding-top: .5rem;
    padding-bottom: 1rem;
    padding-left:1rem;
    margin: 0;
  }
  code {
    font-family: 'Roboto Mono';
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
