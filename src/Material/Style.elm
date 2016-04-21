module Material.Style
  ( Style
  , cs, cs', css, css', attribute, multiple
  , styled, div, span, stylesheet
  ) where


{-| Styling Material components.

Use these to customize components and add your own classes and css to
Material container elements.

(This mechanism is necessary because Elm does not provide a good way to
add to or remove from the contents of an already constructed class Attribute.)

@docs Style

# Constructors
@docs cs, cs', css, css', attribute, multiple

# Application
@docs styled, div, span

# Convenience
@docs stylesheet
-}


import String 

import Html exposing (Html, Attribute)
import Html.Attributes


-- STYLING


{-| Type of Style information. 
-}
type Style
  = Class String
  | CSS (String, String)
  | Attr Html.Attribute
  | Multiple (List Style)
  | NOP


type alias Summary = 
  { attrs : List Attribute 
  , classes : List String 
  , css : List (String, String)  
  }


collect1 : Style -> Summary -> Summary
collect1 style ({ classes, css, attrs } as acc) = 
  case style of 
    Class x -> { acc | classes = x :: classes }
    CSS x -> { acc | css = x :: css }
    Attr x -> { acc | attrs = x :: attrs }
    Multiple styles -> List.foldl collect1 acc styles 
    NOP -> acc


collect : List Style -> Summary 
collect =
  List.foldl collect1 { classes=[], css=[], attrs=[] } 


{-| Handle the common case of setting attributes of a standard Html node
from a List Style. Use like this:

    import Material.Style exposing (..)

    myDiv : Html
    myDiv =
      styled div
        [ css "classA", css "classB" ]
        [ text "This is my div with classes classA and classB!" ]

-}
styled : (List Attribute -> a) -> List Style -> a
styled ctor styles = 
  let
      { classes, css, attrs }  = collect styles
  in
    ctor
      (  Html.Attributes.style css
      :: Html.Attributes.class (String.join " " classes)
      :: attrs
      )


{-| Convenience function for the ultra-common case of setting attributes of a
div element. Use like this: 

    myDiv : Html 
    myDiv = 
      Style.div
        [ Color.background Color.primary
        , Color.text Color.accentContrast
        ]
        [ text "I'm in color!" ]

-}
div : List Style -> List Html -> Html
div styles elems = 
  styled Html.div styles elems


{-| Convenience function for the reasonably common case of setting attributes
of a span element. See also `div`. 
-}
span : List Style -> List Html -> Html
span styles elems = 
  styled Html.span styles elems


{-| Add a HTML class to a component. (Name chosen to avoid clashing with
Html.Attributes.class.)
-}
cs : String -> Style
cs c = Class c


{-| Conditionally add a HTML class to a component.
-}
cs' : String -> Bool -> Style
cs' c b =
  if b then Class c else NOP


{-| Add a CSS style to a component.
-}
css : String -> String -> Style
css key value =
  CSS (key, value)


{-| Add a custom attribute
-}
attribute : Html.Attribute -> Style
attribute attr = 
  Attr attr


{-| Add a custom attribute
-}
multiple : List Style -> Style
multiple styles =
  Multiple (styles)


{-| Conditionally add a CSS style to a component
-}
css' : String -> String -> Bool -> Style
css' key value b =
  if b then CSS (key, value) else NOP


{-| Add a style that does nothing. 
-}
nop : Style 
nop = NOP


-- CONVENIENCE


{-| Construct an Html element contributing to the global stylesheet.
The resulting Html is a `<style>` element.  Remember to insert the resulting Html
somewhere. 
-}
stylesheet : String -> Html
stylesheet css = 
  Html.node "style" [] [Html.text css]
