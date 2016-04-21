module Material.Helpers where

import Html
import Html.Attributes
import Effects exposing (Effects)
import Time exposing (Time)
import Task

filter : (a -> List b -> c) -> a -> List (Maybe b) -> c
filter elem attr html =
  elem attr (List.filterMap (\x -> x) html)


effect : Effects b -> a -> (a, Effects b)
effect e x = (x, e)


pure : a -> (a, Effects b)
pure = effect Effects.none


addFx : Effects a -> (model, Effects a) -> (model, Effects a)
addFx effect1 (model, effect2) =
  (model, Effects.batch [effect1, effect2])

mapFx : (a -> b) -> (model, Effects a) -> (model, Effects b)
mapFx f (model, effect) =
  (model, Effects.map f effect)

clip : comparable -> comparable -> comparable -> comparable
clip lower upper k = Basics.max lower (Basics.min k upper)


blurOn : String -> Html.Attribute
blurOn evt =
  Html.Attributes.attribute ("on" ++ evt) <| "this.blur()"


-- TUPLES


map1 : (a -> a') -> (a, b, c) -> (a', b, c)
map1 f (x,y,z) = (f x, y, z)


map2 : (b -> b') -> (a, b, c) -> (a, b', c)
map2 f (x,y,z) = (x, f y, z)


map1st : (a -> c) -> (a,b) -> (c,b)
map1st f (x,y) = (f x, y)


map2nd : (b -> c) -> (a,b) -> (a,c)
map2nd f (x,y) = (x, f y)


{- Variant of EA update function type, where effects may be 
lifted to a different type. 
-}
type alias Update' model action action' = 
  action -> model -> (model, Effects action')


{-| Standard EA update function type. 
-}
type alias Update model action = 
  Update' model action action


lift' :
  (model -> submodel) ->                                      -- get
  (model -> submodel -> model) ->                             -- set
  (subaction -> submodel -> submodel) -> 
  subaction ->                                                -- action
  model ->                                                    -- model
  (model, Effects action)
lift' get set update action model =
  (set model (update action (get model)), Effects.none)

lift :
  (model -> submodel) ->                                      -- get
  (model -> submodel -> model) ->                             -- set
  (subaction -> action) ->                                    -- fwd
  Update submodel subaction ->                               -- update
  subaction ->                                                -- action
  model ->                                                    -- model
  (model, Effects action)
lift get set fwd update action model =
  let
    (submodel', e) = update action (get model)
  in
    (set model submodel', Effects.map fwd e)


fx : a -> Effects a
fx =
  Task.succeed >> Effects.task


delay : Time -> a -> Effects a
delay t x =
  Task.sleep t
    |> (flip Task.andThen) (always (Task.succeed x))
    |> Effects.task


