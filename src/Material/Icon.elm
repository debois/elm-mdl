module Material.Icon
  ( size18, size24, size36, size48
  , view
  , i
  ) where


{-| Convenience functions for producing Material Design Icons. Refer to
[the Material Design Icons page](https://google.github.io/material-design-icons),
or skip straight to the [Material Icons Library](https://design.google.com/icons/).

This implementation assumes that you have

    <link href="https://fonts.googleapis.com/icon?family=Material+Icons"
      rel="stylesheet">

or an equivalent means of loading the icons in your HTML header. 
(`Material.top` will do this for you.)

@docs i, view, size18, size24, size36, size48
-}


import Html exposing (i, text, Html, Attribute)
import Material.Style exposing (Style, cs, css, styled)


{-| Set icon to have size 18px. 
-}
size18 :  Style
size18 = 
  css "font-size" "18px"


{-| Set icon to have size 24px. 
-}
size24 :  Style
size24 = 
  css "font-size" "24px"


{-| Set icon to have size 36px. 
-}
size36 :  Style
size36 = 
  css "font-size" "36px"
  

{-| Set icon to have size 48px. 
-}
size48 :  Style
size48 = 
  css "font-size" "48px"


{-| View function for icons. Supply the
[Material Icons Library](https://design.google.com/icons/) name as
the first argument (replace spaces with underscores). Set the size of the
icon with the `size..` Styles (default is 24px).  Do not use this function to
produce clickable icons; use icon buttons in Material.Button.icon for that.

    import Material.Icon as Icon

    icon : Html
    icon = Icon.view "trending_flat" [Icon.size48] []
-}
view : String -> List Style -> Html
view name styling =
  styled Html.i
    (  cs "material-icons"
    :: styling
    )
    [text name]

  
{-| Render a default-sized icon with no behaviour. The
`String` argument must be the name of a [Material Icon](https://design.google.com/icons/)
(replace spaces with underscores).

I.e., to produce a default size (24xp) "trending flat" icon:

    import Material.Icon as Icon

    icon : Html
    icon = Icon.i "trending_flat"
-}
i : String -> Html
i name = view name []
