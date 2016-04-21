module Material.Grid
  ( grid, noSpacing, maxWidth
  , Cell, cell
  , Device(..)
  , Align(..)
  , size
  , offset
  , align
  , hide
  , order
  ) where

{-| From the
[Material Design Lite documentation](http://www.getmdl.io/components/#layout-section/grid):

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

Refer to 
[this site](https://debois.github.io/elm-mdl/#/grid)
for a live demo. 

Example use:

    import Material.Grid exposing (grid, cell, size, Device(..))

    top : Html
    top =
      grid []
        [ cell [ size All 4 ]
            [ h4 [] [text "Cell 1"]
            ]
        , cell [ offset All 2, size All 4 ]
            [ h4 [] [text "Cell 2"]
            , p [] [text "This cell is offset by 2"]
            ]
        , cell [ size All 6 ]
            [ h4 [] [text "Cell 3"]
            ]
        , cell [ size Tablet 6, size Desktop 12, size Phone 2 ]
            [ h4 [] [text "Cell 4"]
            , p [] [text "Size varies with device"]
            ]
        ]

# Grid container
@docs grid, noSpacing, maxWidth

# Cells

Cells are configured with a `List Style`; this configuration dictates the
size, offset, etc. of the cell. 

@docs cell, Cell, Device, size, offset, Align, align, hide, order
-}


{- TODO. I don't understand what "mdl-cell--stretch" or when it might be appropriate.
-}


import Html exposing (..)

import Material.Helpers exposing (clip, filter)
import Material.Style as Style exposing (Style, cs, styled)


{-| Set grid to have no spacing between cells. 
-}
noSpacing : Style 
noSpacing = Style.cs "mdl-grid--no-spacing"

{-| Set maximum grid width. If more space is available, the grid stays centered with
padding on either side. Width must be a valid CSS dimension. 
-}
maxWidth : String -> Style 
maxWidth w = Style.css "max-width" w

{-| Construct a grid with options.
-}
grid : List Style -> List Cell -> Html
grid styling cells =
  Style.div (cs "mdl-grid" :: styling) (List.map (\(Cell elm) -> elm) cells)


{-| Device specifiers, used with `size` and `offset`. (A `Device` really
encapsulates a screen size.)
-}
type Device = All | Desktop | Tablet | Phone


{-| Opaque cell type.
-}
type Cell = Cell Html


suffix : Device -> String
suffix device =
  case device of
    All -> ""
    Desktop -> "-desktop"
    Tablet -> "-tablet"
    Phone -> "-phone"


{-| Specify cell size. On devices of type `Device`, the
cell being specified spans `Int` columns.
-}
size : Device -> Int -> Style
size device k =
  let c =
    case device of
      All -> clip 1 12 k
      Desktop -> clip 1 12 k
      Tablet -> clip 1 8 k
      Phone -> clip 1 4 k
  in
    "mdl-cell--" ++ toString c ++ "-col" ++ suffix device |> cs


{-| Specify cell offset, i.e., empty number of empty cells before the present
one. On devices of type `Device`, leave `Int` columns blank before the present
one begins.
-}
offset : Device -> Int -> Style
offset device k =
  let c =
    case device of
      All -> clip 1 11 k
      Desktop -> clip 1 11 k
      Tablet -> clip 1 7 k
      Phone -> clip 1 3 k
  in
    "mdl-cell--" ++ toString c ++ "-offset" ++ suffix device |> cs


{-| Alignment of cell contents; use with `align`.
-}
type Align = Top | Middle | Bottom 


{-| Specify vertical cell alignment. See `Align`.
-}
align : Align -> Style
align a =
  case a of 
    Top -> cs "mdl-cell--top"
    Middle -> cs "mdl-cell--middle"
    Bottom -> cs "mdl-cell--bottom"



{-| Specify that a cell should be hidden on given `Device`.
-}
hide : Device -> Style
hide device =
  cs <| case device of
    All -> ""
    _ -> "mdl-cell--hide-" ++ suffix device


{-| Specify that a cell should re-order itself to position 'Int' on `Device`.
-}
order : Device -> Int -> Style
order device n =
  cs <| "mdl-cell--order-" ++ (toString <| clip 1 12 n) ++ suffix device


{-| Construct a cell for use in the argument list for `grid`. Note that this
module defines various styles to set size, offset, etc. of the cell. 
-}
cell : List Style -> List Html -> Cell
cell styling elms =
  Cell (Style.div (cs "mdl-cell" :: styling) elms)
