module Material.Menu.Oracle
  ( Oracle
  , decode
  , Element
  , Rect
  , test
  ) where

import Effects exposing (Effects, none)
import Html exposing (..)
import Html.Attributes as Html
import Html.Events as Html exposing (defaultOptions)
import Json.Encode exposing (string)
import Json.Decode as Json exposing (Decoder, list, at, (:=), andThen)
import Task
import Native.Menu
import DOM
import String

import Native.Menu

type alias Element =
  { offsetTop : Float
  , offsetLeft : Float
  , offsetHeight : Float
  , bounds : Rect
  }

type alias Rect =
  { top   : Float
  , left  : Float
  , bottom: Float
  , right : Float
  , width : Float
  , height: Float
  }

--defaultRect =
--  { top = 0
--  , left = 0
--  , bottom = 0
--  , right = 0
--  , width = 0
--  , height = 0
--  }

type alias Oracle =
  { button : Element
  , menu : Element
  , container : Element
  , offsetTops : List Float
  , offsetHeights : List Float
  }

test =
  DOM.target DOM.boundingClientRect

nodeList : Decoder a -> Decoder (List a)
nodeList decode =
  Native.decodeNodeList decode

decode : Decoder Oracle
decode =
  Json.object5
    ( \button menu container offsetTops offsetHeights ->
        
        { button = button
        , menu   = menu
        , container = container
        , offsetTops = offsetTops
        , offsetHeights = offsetHeights
        }
    )
    ("target" := element `andThen` (\target -> Json.succeed (Debug.log "target" target)))
    (at ["target", "nextSibling[1]"] element)
    (at ["target","nextSibling"] element)
    (mapItems DOM.offsetTop)
    (mapItems DOM.offsetHeight)

mapItems decoder =
  at ["target", "nextSibling[1]", "childNodes"] (list decoder)

element =
  Json.object4
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
    boundingClientRect

boundingClientRect =
  Json.object3
    (\(x, y) width height ->
      { top = y
      , left = x
      , bottom = 0
      , right = 0
      , width = width
      , height = height
      })
    (position 0 0)
    DOM.offsetWidth
    DOM.offsetHeight

position : Float -> Float -> Json.Decoder (Float, Float)
position x y =
  Json.object4
    (\scrollLeft scrollTop offsetLeft offsetTop ->
      (x + offsetLeft - scrollLeft, y + offsetTop - scrollTop))
      DOM.scrollLeft
      DOM.scrollTop
      DOM.offsetLeft
      DOM.offsetTop
   `andThen` (\(x',y') ->
      DOM.offsetParent (x', y') (position x' y')
   )
