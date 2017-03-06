module Material.Chip
    exposing
        ( Property
        , HtmlElement
        , Content
        , chip
        , button
        , span
        , content
        , text
        , contact
        , deleteIcon
        , deleteLink
        , deleteClick
        )

{-| From the [Material Design Lite documentation](http://www.getmdl.io/components/index.html#chips-section):

> The Material Design Lite (MDL) chip component is a small, interactive element.
> Chips are commonly used for contacts, text, rules, icons, and photos.

See also the
[Material Design Specification](https://www.google.com/design/spec/components/chips.html).

Refer to [this site](http://debois.github.io/elm-mdl/#chips)
for a live demo.


# Types
@docs Property, HtmlElement, Content

# Elements
@docs chip, button, span
@docs content , text , contact

# Properties
@docs deleteIcon , deleteLink , deleteClick

-}

import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Events
import Material.Options as Options exposing (cs)
import Material.Icon as Icon
import Material.Options.Internal as Internal
import Material.Helpers as Helpers
import Json.Decode as Json


{-| Alias for a `Html m` function. e.g. `Html.div`
-}
type alias HtmlElement msg =
    List (Attribute msg) -> List (Html msg) -> Html msg


type alias Config msg =
    { deleteIcon : Maybe String
    , deleteLink : Maybe (Html.Attribute msg)
    , deleteClick : Maybe (Html.Attribute msg)
    }


defaultConfig : Config msg
defaultConfig =
    { deleteIcon = Nothing
    , deleteLink = Nothing
    , deleteClick = Nothing
    }


{-| Properties for Chip options.
-}
type alias Property msg =
    Options.Property (Config msg) msg


{-| Chip can contain only specific kind of content
-}
type Content msg
    = Contact (HtmlElement msg) (List (Property msg)) (List (Html msg))
    | Text (List (Property msg)) (List (Html msg))
    | Action (HtmlElement msg) (List (Property msg)) (List (Html msg))


{-| Set the icon for the delete action
-}
deleteIcon : String -> Property msg
deleteIcon =
    Internal.option
        << (\icon config -> { config | deleteIcon = Just icon })


{-| Set the link for the delete action.

NOTE. This turns the action to `Html.a` element
-}
deleteLink : String -> Property msg
deleteLink =
    Internal.option
        << (\link config -> { config | deleteLink = Just (Html.Attributes.href link) })


{-| Set the `onClick` for the delete action

NOTE. This stops propagation and prevents default to stop `onClick` from being
called when this is clicked.
-}
deleteClick : msg -> Property msg
deleteClick =
    Internal.option
        << (\msg config ->
                { config
                    | deleteClick =
                        Just
                            (Html.Events.onWithOptions "click"
                                { stopPropagation = True, preventDefault = True }
                                (Json.succeed msg)
                            )
                }
           )

type alias Priority =
    Int


priority : Content a -> Priority
priority item =
    case item of
        Contact _ _ _ ->
            0

        Text _ _ ->
            1

        Action _ _ _ ->
            2


{-| Renders a given `Content`
-}
renderItem : Content msg -> Html msg
renderItem item =
    case item of
        Contact element props content ->
            Options.styled element
                (cs "mdl-chip__contact" :: props)
                content

        Text props content ->
            Options.styled Html.span
                (cs "mdl-chip__text" :: props)
                content

        Action element props content ->
            Options.styled element
                (cs "mdl-chip__action" :: props)
                content


hasValue : Maybe a -> Bool
hasValue m =
    case m of
        Just _ ->
            True

        Nothing ->
            False


getActionElement : Config msg -> Maybe (Content msg)
getActionElement config =
    let
        hasIcon =
            hasValue config.deleteIcon

        hasLink =
            hasValue config.deleteLink

        hasClick =
            hasValue config.deleteClick

        icon =
            if hasIcon then
                Maybe.withDefault "" config.deleteIcon
            else if (hasLink || hasClick) then
                Maybe.withDefault "cancel" config.deleteIcon
            else
                ""

        actionElement =
            if hasLink then
                Html.a
            else
                Html.span

        link =
            case config.deleteLink of
                Just l ->
                    Internal.attribute l

                Nothing ->
                    Options.nop

        click =
            case config.deleteClick of
                Just c ->
                    Internal.attribute c

                Nothing ->
                    Options.nop

        isDeletable =
            hasIcon || hasLink || hasClick
    in
        if isDeletable then
            action actionElement
                [ link, click ]
                [ Icon.view icon [] ]
                |> Just
        else
            Nothing


{-| Creates a chip using `Html.button`
-}
button : List (Property msg) -> List (Content msg) -> Html msg
button props =
    chip Html.button ((Internal.attribute <| Html.Attributes.type_ "button") :: props)


{-| Creates a chip using `Html.span`
-}
span : List (Property msg) -> List (Content msg) -> Html msg
span =
    chip Html.span


{-| Create a chip contained in the given element
-}
chip : HtmlElement msg -> List (Property msg) -> List (Content msg) -> Html msg
chip element props items =
    let
        summary =
            Internal.collect defaultConfig props

        config =
            summary.config

        action =
            getActionElement config

        isDeletable =
            hasValue action

        withIcon =
            (case action of
                Just a ->
                    [ a ]

                Nothing ->
                    []
            )
                ++ items

        content =
            withIcon
                |> List.sortBy priority
                |> List.map renderItem

        isContact =
            List.any (\x -> priority x == 0) items
    in
        Options.styled element
            ([ cs "mdl-chip"
             , Options.when isContact (cs "mdl-chip--contact")
             , Options.when isDeletable (cs "mdl-chip--deletable")
             , Internal.attribute <| Helpers.blurOn "mouseup"
             , Internal.attribute <| Helpers.blurOn "mouseleave"
             , Internal.attribute <| Helpers.blurOn "touchend"
             ]
                ++ props)
            content


{-| Generate chip content
-}
content : List (Property msg) -> List (Html msg) -> Content msg
content =
    Text


{-| Shorthand for `Chip.content [] [ Html.text "text" ]`
-}
text : List (Property msg) -> String -> Content msg
text props txt =
    Text props [ Html.text txt ]


{-| Create a chip action contained in the given element
-}
action : HtmlElement msg -> List (Property msg) -> List (Html msg) -> Content msg
action =
    Action


{-| Create a chip contact contained in the given element
-}
contact : HtmlElement msg -> List (Property msg) -> List (Html msg) -> Content msg
contact =
    Contact
