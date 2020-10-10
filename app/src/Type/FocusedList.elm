module Type.FocusedList exposing (..)
import Dict exposing (Dict)
import Array exposing (Array)

type FocusedList a = FocusedList
    { mapping: Dict String a
    , arr: Array String
    , focus: Maybe String
    , keyFoo: a -> String
    }

fromArray : Array a -> (a -> String) -> FocusedList a
fromArray arr keyFoo =
    let
        m = Array.foldl (\d -> Dict.insert (keyFoo d) d) Dict.empty arr 
    in FocusedList
        { mapping = m
        , arr = Array.map keyFoo arr
        , focus = Nothing
        , keyFoo = keyFoo
        }

focusOn : FocusedList a -> String -> FocusedList a
focusOn (FocusedList fl) key = if Dict.member key fl.mapping
    then FocusedList {fl | focus = Just key}
    else FocusedList fl

update : FocusedList a -> a -> FocusedList a
update (FocusedList fl) value =
    let
        key = fl.keyFoo value
    in if Dict.member key fl.mapping
        then FocusedList {fl | mapping = Dict.insert key value fl.mapping}
        else FocusedList fl

appendIfNew : FocusedList a -> a -> FocusedList a
appendIfNew (FocusedList fl) value =
    let
        key = fl.keyFoo value
    in if Dict.member key fl.mapping
        then FocusedList fl
        else FocusedList {fl
            | mapping = Dict.insert key value fl.mapping
            , arr = Array.append (Array.fromList [key]) fl.arr
            }

find : FocusedList a -> String -> Maybe a
find (FocusedList fl) key = Dict.get key fl.mapping 

mergeNew : FocusedList a -> Array a -> FocusedList a
mergeNew (FocusedList fl) arr =
    let
        updater : a -> Maybe a -> Maybe a
        updater d v = case v of
           Just x  -> Just x
           Nothing -> Just d
        m = Array.foldl (\d -> Dict.update (fl.keyFoo d) (updater d)) fl.mapping arr 
    in FocusedList { fl
        | mapping = m
        , arr = Array.map fl.keyFoo arr
        }

listWithFocus : FocusedList a -> (a -> Bool -> b) -> List b 
listWithFocus (FocusedList fl) foo =
    let
        getter key xs = case Dict.get key fl.mapping of
           Just v  -> foo v (Just key == fl.focus) :: xs
           Nothing -> xs 
    in Array.foldl getter [] fl.arr

getFocusKey : FocusedList a -> Maybe String
getFocusKey (FocusedList fl) = fl.focus

getFocus : FocusedList a -> Maybe a
getFocus (FocusedList fl) = Maybe.andThen (\k -> Dict.get k fl.mapping) fl.focus 