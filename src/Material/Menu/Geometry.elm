module Material.Menu.Geometry
  ( Geometry, Element
  , decode, decode'
  ) where

import Array exposing (Array)
import DOM
import Json.Decode exposing (..)

{-| An Geometry stores relevant information from DOM during Open and Close
events. (This computes more than it needs to.)
-}
type alias Geometry =
  { button : Element
  , menu : Element
  , container : Element
  , offsetTops : Array Float
  , offsetHeights : Array Float
  }


type alias Element =
  { offsetTop : Float
  , offsetLeft : Float
  , offsetHeight : Float
  , bounds : DOM.Rectangle
  }


{-| Decode Geometry from the button's reference
-}
decode : Decoder Geometry
decode =
  object5 Geometry
    (DOM.target element)
    (DOM.target (DOM.nextSibling (DOM.childNode 1 element)))
    (DOM.target (DOM.nextSibling  element))
    (DOM.target (DOM.nextSibling (DOM.childNode 1 (DOM.childNodes DOM.offsetTop `andThen` (succeed << Array.fromList)))))
    (DOM.target (DOM.nextSibling (DOM.childNode 1 (DOM.childNodes DOM.offsetHeight `andThen` (succeed << Array.fromList)))))


{-| Decode Geometry from a menu item's reference
-}
decode' : Decoder Geometry
decode' =
  object5 Geometry
    (DOM.target (DOM.parentElement (DOM.parentElement (DOM.previousSibling element))))
    (DOM.target (DOM.parentElement element))
    (DOM.target (DOM.parentElement (DOM.parentElement element)))
    (DOM.target (DOM.parentElement (DOM.childNodes DOM.offsetTop `andThen` (succeed << Array.fromList))))
    (DOM.target (DOM.parentElement (DOM.childNodes DOM.offsetHeight `andThen` (succeed << Array.fromList))))


{-| Decode an Element
-}
element : Decoder Element
element =
  object4 Element
    DOM.offsetTop
    DOM.offsetLeft
    DOM.offsetHeight
    DOM.boundingClientRect
