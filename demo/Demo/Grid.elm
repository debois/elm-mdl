module Demo.Grid where


import Html exposing (..)
import Array

import Material.Grid exposing (..)
import Material.Style exposing (Style, css)
import Material.Color as Color 

import Demo.Page as Page


-- Cell styling


style : Int -> List Style
style h = 
  [ css "text-sizing" "border-box"
  , css "background-color" "#BDBDBD"
  , css "height" (toString h ++ "px")
  , css "padding-left" "8px"
  , css "padding-top" "4px"
  , css "color" "white"
  ]


-- Cell variants


democell : Int -> List Style -> List Html -> Cell
democell k styling = 
  cell <| List.concat [style k, styling] 

  
small : List Style -> List Html -> Cell
small = democell 50


std : List Style -> List Html -> Cell
std = democell 200


-- Grid 


color : Int -> Style
color k =
    Array.get ((k + 0) % Array.length Color.hues) Color.hues
      |> Maybe.withDefault Color.Teal
      |> flip Color.color Color.S500
      |> Color.background


view : Html
view =
  [ p []
      [ text """Resize your browser-window and observe the effect on the Grid
                below. Note in particular what happens to the top and bottom rows."""
      ]
  , [1..12 ]
    |> List.map (\i -> small [size All 1, color 4] [text "1"])
    |> grid []
  , [1 .. 3]
    |> List.map (\i -> std [size All 4, color 5] [text <| "4"])
    |> grid []
  , [ std [size All 6, color 6] [text "6"]
    , std [size All 4, color 6] [text "4"]
    , std [size All 2, color 6] [text "2"]
    ] |> grid []
  , [ std [size All 6, size Tablet 8, color 7] [text "6 (8 tablet)"]
    , std [size All 4, size Tablet 6, color 8] [text "4 (6 tablet)"]
    , std [size All 2, size Phone 4,  color 9] [text "2 (4 phone)"]
    ] |> grid []
  ]
  |> Page.body1 "Grid" srcUrl intro references


intro : Html
intro = 
  Page.fromMDL "http://www.getmdl.io/components/#layout-section/grid" """
> The Material Design Lite (MDL) grid component is a simplified method for laying
> out content for multiple screen sizes. It reduces the usual coding burden
> required to correctly display blocks of content in a variety of display
> conditions.
>
> The MDL grid is defined and enclosed by a container element. A grid has 12
> columns in the desktop screen size, 8 in the tablet size, and 4 in the phone
> size, each size having predefined margins and gutters. Cells are laid out
> sequentially in a row, in the order they are defined, with some exceptions:
>
>   - If a cell doesn't fit in the row in one of the screen sizes, it flows
>     into the following line.
>   - If a cell has a specified column size equal to or larger than the number
>     of columns for the current screen size, it takes up the entirety of its
>     row.
"""

srcUrl : String
srcUrl = 
  "https://github.com/debois/elm-mdl/blob/master/demo/Demo/Grid.elm"

references : List (String, String) 
references = 
  [ Page.package "http://package.elm-lang.org/packages/debois/elm-mdl/latest/Material-Grid"
  , Page.mds "https://www.google.com/design/spec/layout/responsive-ui.html#responsive-ui-grid"
  , Page.mdl "http://www.getmdl.io/components/#layout-section/grid"
  ]
