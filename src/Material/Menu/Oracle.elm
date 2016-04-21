module Material.Menu.Oracle
  ( Oracle, Element
  , decode, decode'
  ) where

import DOM
import Json.Decode exposing (..)

{-| An Oracle stores relevant information from DOM during Toggle and Close
events. This computes more than it needs to.
-}

type alias Oracle =
  { button : Element
  , menu : Element
  , container : Element
  , offsetTops : List Float
  , offsetHeights : List Float
  }

type alias Element =
  { offsetTop : Float
  , offsetLeft : Float
  , offsetHeight : Float
  , bounds : DOM.Rectangle
  }

{-| Decode Oracle from the button's reference
-}

decode : Decoder Oracle
decode =
  object5
    ( \button container menu offsetTops offsetHeights ->

        { button = button
        , menu   = menu
        , container = container
        , offsetTops = offsetTops
        , offsetHeights = offsetHeights
        }
    )
    ("target" := element)
    (at ["target", "nextSibling"]  element)
    (at ["target", "nextSibling"] (DOM.childNode 1 element))
    (at ["target", "nextSibling"] (DOM.childNode 1 (DOM.childNodes DOM.offsetTop)))
    (at ["target", "nextSibling"] (DOM.childNode 1 (DOM.childNodes DOM.offsetHeight)))

{-| Decode Oracle from a menu item's reference
-}

decode' : Decoder Oracle
decode' =
  object5
    ( \button container menu offsetTops offsetHeights ->

        { button = button
        , container = container
        , menu   = menu
        , offsetTops = offsetTops
        , offsetHeights = offsetHeights
        }
    )
    (at ["target", "parentNode", "parentNode", "previousSibling"] element)
    (at ["target", "parentNode", "parentNode"] element)
    (at ["target", "parentNode"] element)
    (at ["target", "parentNode"] (DOM.childNodes DOM.offsetTop))
    (at ["target", "parentNode"] (DOM.childNodes DOM.offsetHeight))

{-| Decode an Element
-}

element : Decoder Element
element =
  object4
    (\offsetTop offsetLeft offsetHeight bounds ->

        { offsetTop = offsetTop
        , offsetLeft = offsetLeft
        , offsetHeight = offsetHeight
        , bounds = bounds
        }
    )
    DOM.offsetTop
    DOM.offsetLeft
    DOM.offsetHeight
    DOM.boundingClientRect
