module Material.Tabs
    exposing
        ( Label
        , Property
        , Msg
        , render
        , update
        , view
        , label
        , textLabel
        , ripple
        , onSelectTab
        , activeTab
        , Model
        , defaultModel
        , react
        )

{-| From the [Material Design Lite documentation](https://getmdl.io/components/index.html#layout-section/tabs):

> The Material Design Lite (MDL) tab component is a user interface element that
> allows different content blocks to share the same screen space in a mutually
> exclusive manner. Tabs are always presented in sets of two or more, and they
> make it easy to explore and switch among different views or functional aspects
> of an app, or to browse categorized data sets individually. Tabs serve as
> "headings" for their respective content; the active tab — the one whose content
> is currently displayed — is always visually distinguished from the others so the
> user knows which heading the current content belongs to.
>
> Tabs are an established but non-standardized feature in user interfaces, and
> allow users to view different, but often related, blocks of content (often
> called panels). Tabs save screen real estate and provide intuitive and logical
> access to data while reducing navigation and associated user confusion. Their
> design and use is an important factor in the overall user experience. See the
> tab component's Material Design specifications page for details.

See also the
[Material Design Specification](https://material.google.com/components/tabs.html#tabs-usage).

Refer to [this site](http://debois.github.io/elm-mdl/#tabs)
for a live demo.

# Types
@docs Label
@docs Property

# Render

@docs render


# Events

@docs onSelectTab, activeTab


# Appearance

@docs ripple


# Content

@docs label
@docs textLabel


# Elm architecture

@docs Model, defaultModel
@docs Msg
@docs update
@docs view

# Internal use
@docs react
-}

import Platform.Cmd exposing (Cmd, none)
import Html exposing (Html)
import Material.Component as Component exposing (Indexed, Index)
import Material.Options as Options exposing (cs, when)
import Material.Options.Internal as Internal
import Material.Ripple as Ripple
import Html.Attributes exposing (class)
import Html.Keyed as Keyed
import Dict exposing (Dict)


-- MODEL


{-| Component model.
-}
type alias Model =
    { ripples : Dict Int Ripple.Model
    }


{-| Default component model constructor.
-}
defaultModel : Model
defaultModel =
    { ripples = Dict.empty
    }



-- ACTION, UPDATE


{-| Component action.
-}
type Msg
    = Ripple Int Ripple.Msg


{-| Component update.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Ripple tabIdx action_ ->
            let
                ( ripple_, cmd ) =
                    Dict.get tabIdx model.ripples
                        |> Maybe.withDefault Ripple.model
                        |> Ripple.update action_
            in
                ( { model | ripples = Dict.insert tabIdx ripple_ model.ripples }, Cmd.map (Ripple tabIdx) cmd )



-- PROPERTIES


type alias Config m =
    { ripple : Bool
    , onSelectTab : Maybe (Int -> m)
    , activeTab : Int
    }


defaultConfig : Config m
defaultConfig =
    { ripple = False
    , onSelectTab = Nothing
    , activeTab = 0
    }


{-| Tab options.
-}
type alias Property m =
    Options.Property (Config m) m


{-| Opaque `Label` type
-}
type Label m
    = Label ( List (Property m), List (Html m) )


{-| Create tab `label`
-}
label : List (Property m) -> List (Html m) -> Label m
label p c =
    Label ( p, c )


{-| Create tab `label` with simple text.
Most often the labels are just text so this is a
utility function to help create labels with just text.
-}
textLabel : List (Property m) -> String -> Label m
textLabel p c =
    label p [ Html.text c ]


{-| Make tabs ripple when clicked.
-}
ripple : Property m
ripple =
    Internal.option (\config -> { config | ripple = True })


{-| Receieve notification when tab `k` is selected.
-}
onSelectTab : (Int -> m) -> Property m
onSelectTab =
    Internal.option << (\k config -> { config | onSelectTab = Just k })


{-| Set the active tab.
-}
activeTab : Int -> Property m
activeTab =
    Internal.option << (\k config -> { config | activeTab = k })



-- VIEW


{-| Component view.
-}
view : (Msg -> m) -> Model -> List (Property m) -> List (Label m) -> List (Html m) -> Html m
view lift model options tabs tabContent =
    let
        summary =
            Internal.collect defaultConfig options

        config =
            summary.config

        -- Wraps the tab content into a proper tab panel
        -- Always active because the visible tab is always active.
        {- Keyed to prevent scrolling state being retained when we switch tab.
           This does cause scrolling to reset when changing tabs.
        -}
        wrapContent =
            Keyed.node "div"
                [ Html.Attributes.classList
                    [ ( "mdl-tab__panel", True )
                    , ( "is-active", True )
                    ]
                ]

        unwrapLabel tabIdx (Label ( props, content )) =
            Options.styled Html.a
                [ cs "mdl-tabs__tab"
                , cs "is-active" |> when (tabIdx == config.activeTab)
                , config.onSelectTab
                    |> Maybe.map (\t -> Options.onClick (t tabIdx))
                    |> Maybe.withDefault Options.nop
                , Options.many props
                ]
                (if config.ripple then
                    List.concat
                        [ content
                        , [ Ripple.view
                                [ Html.Attributes.classList
                                    [ ( "mdl-tabs__ripple-container", True )
                                    , ( "mdl-tabs__ripple-js-effect", True )
                                    ]
                                ]
                                (Dict.get tabIdx model.ripples
                                    |> Maybe.withDefault Ripple.model
                                )
                                |> Html.map (Ripple tabIdx >> lift)
                          ]
                        ]
                 else
                    content
                )

        links =
            Options.styled Html.div
                [ cs "mdl-tabs__tab-bar"
                ]
                (List.indexedMap unwrapLabel tabs)
    in
        Internal.apply summary
            Html.div
            [ cs "mdl-tabs"
            , cs "mdl-js-tabs"
            , cs "is-upgraded"
            , when config.ripple (cs "mdl-js-ripple-effect")
            , when config.ripple (cs "mdl-js-ripple-effect--ignore-events")
            ]
            []
            [ links
            , (wrapContent [ ( toString config.activeTab, Html.div [] tabContent ) ])
            ]



-- COMPONENT


type alias Store s =
    { s | tabs : Indexed Model }


( get, set ) =
    Component.indexed .tabs (\x y -> { y | tabs = x }) defaultModel


{-| Component react function.
-}
react :
    (Component.Msg button textfield menu layout toggles tooltip Msg select dispatch -> m)
    -> Msg 
    -> Index
    -> Store s
    -> ( Maybe (Store s), Cmd m )
react =
    Component.react get set Component.TabsMsg (Component.generalise update)


{-| Component render.
-}
render :
    (Component.Msg button textfield menu snackbar toggles tooltip Msg select dispatch -> m)
    -> Component.Index
    -> Store s
    -> List (Property m)
    -> List (Label m)
    -> List (Html m)
    -> Html m
render =
    Component.render get view Component.TabsMsg
