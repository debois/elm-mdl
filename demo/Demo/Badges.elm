module Demo.Badges (..) where

import Html exposing (..)
import Material.Badge as Badge
import Material.Style as Style exposing (styled)
import Material.Icon as Icon
import Material.Grid exposing (..)

import Demo.Page as Page


-- VIEW


c : List Html -> Cell
c = cell [ size All 4 ]  

view : Html
view =
  [ p []
      [ text """Below are examples of various badges."""
      ]
  , grid 
      [] 
      [ c [Style.span [ Badge.withBadge "2" ]  [text "Badge"]  ]
      , c [Style.span 
            [ Badge.withBadge "22", Badge.noBackground ]  
            [ text "No background" ]
          ]
      , c [Style.span 
            [ Badge.withBadge "33", Badge.overlap ]  
            [ text "Overlap" ]
          ]
      , c [Style.span 
            [ Badge.withBadge "99", Badge.overlap, Badge.noBackground ]  
            [ text "Overlap, no background" ]
          ]
      , c [Style.span 
            [ Badge.withBadge "♥" ]  
            [ text "Symbol" ]
          ]
      , c [ Icon.view "flight_takeoff" [ Icon.size24, Badge.withBadge "33", Badge.overlap ] ]
      ]
  ]
  |> Page.body2 "Badges" srcUrl intro references



intro : Html
intro =
  Page.fromMDL "http://www.getmdl.io/components/#badges-section" """
> The Material Design Lite (MDL) badge component is an onscreen notification
> element. A badge consists of a small circle, typically containing a number or
> other characters, that appears in proximity to another object. A badge can be
> both a notifier that there are additional items associated with an object and
> an indicator of how many items there are.
> 
> You can use a badge to unobtrusively draw the user's attention to items they
> might not otherwise notice, or to emphasize that items may need their
> attention. For example:
> 
>  - A "New messages" notification might be followed by a badge containing the
> number of unread messages.  
>  - A "You have unpurchased items in your shopping cart" reminder might include
>  a badge showing the number of items in the cart.
>  - A "Join the discussion!" button might have an accompanying badge indicating the
> number of users currently participating in the discussion.  
> 
> A badge is almost
> always positioned near a link so that the user has a convenient way to access
> the additional information indicated by the badge. However, depending on the
> intent, the badge itself may or may not be part of the link.
> 
> Badges are a new feature in user interfaces, and provide users with a visual clue to help them discover additional relevant content. Their design and use is therefore an important factor in the overall user experience.
> 
"""


srcUrl : String
srcUrl = 
  "https://github.com/debois/elm-mdl/blob/master/demo/Demo/Badges.elm"


references : List (String, String)
references = 
  [ Page.package "http://package.elm-lang.org/packages/debois/elm-mdl/latest/Material-Badge"
  --, Page.mds "https://www.google.com/design/spec/components/buttons.html"
  , Page.mdl "https://www.getmdl.io/components/#badges-section"
  ]

