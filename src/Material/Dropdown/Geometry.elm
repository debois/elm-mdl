module Material.Dropdown.Geometry
    exposing
        (
          Geometry
        , Element

        , geometry
        , element

        , defaultGeometry
        , defaultElement
        , defaultRectangle
        )

import DOM exposing (Rectangle)
import Json.Decode as Json exposing (Decoder)



{-| A Geometry stores relevant information from DOM during Open and Close
events. (This computes more than it needs to.)
-}
type alias Geometry =
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


-- DECODER


{-| Decode Geomery
-}
geometry : Decoder Geometry
geometry =
    Json.map5 Geometry
        element
        (DOM.nextSibling (DOM.childNode 1 element))
        (DOM.nextSibling element)
        (DOM.nextSibling (DOM.childNode 1 (DOM.childNodes DOM.offsetTop)))
        (DOM.nextSibling (DOM.childNode 1 (DOM.childNodes DOM.offsetHeight)))


{-| Decode an Element
-}
element : Decoder Element
element =
    Json.map4 Element
        DOM.offsetTop
        DOM.offsetLeft
        DOM.offsetHeight
        DOM.boundingClientRect


-- HELPERS


{-| Default geometry. Useful for debugging. -}
defaultGeometry : Geometry
defaultGeometry =
    { button = defaultElement
    , menu = defaultElement
    , container = defaultElement
    , offsetTops = []
    , offsetHeights = []
    }


{-| Default geometry. Useful for debugging. -}
defaultElement : Element
defaultElement =
    { offsetTop  = 0
    , offsetLeft = 0
    , offsetHeight = 0
    , bounds = defaultRectangle
    }


{-| Default rectangle. Useful for debugging. -}
defaultRectangle : Rectangle
defaultRectangle =
    { top = 0, left = 0, width = 0, height = 0 }
