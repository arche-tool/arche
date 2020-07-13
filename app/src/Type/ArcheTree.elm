module Type.ArcheTree exposing (..)

import Array exposing (Array)

import Type.FocusedList exposing (FocusedList)
import Type.FocusedList as FocusedList
import Type.EBSD exposing (EBSD)
import Type.OR exposing (OREval)


type alias EbsdHash = String
type alias OrHash = String

type alias EBSDNode =
    { ebsd: EBSD
    , ors: FocusedList ORNode
    } 

type alias ORNode =
    { orEvaluation: OREval
    } 

type alias ArcheTree = FocusedList EBSDNode

empty : ArcheTree
empty = FocusedList.fromArray Array.empty (\e -> e.ebsd.hashEBSD)

refreshArcheTree : ArcheTree -> Array EBSD -> ArcheTree
refreshArcheTree at les =
    let
        newORNode = FocusedList.fromArray Array.empty (\o -> o.orEvaluation.hashOR)
        nodes = Array.map (\x -> {ebsd = x, ors = newORNode}) les
    in FocusedList.mergeNew at nodes 

refreshORNode : FocusedList ORNode -> Array OREval -> FocusedList ORNode
refreshORNode at les =
    let
        nodes = Array.map (\x -> {orEvaluation = x}) les
    in FocusedList.mergeNew at nodes 

refreshOR : ArcheTree -> EbsdHash -> Array OREval -> ArcheTree
refreshOR at hash lor = case FocusedList.find at hash of 
    Nothing -> at
    Just node ->
        let
            newNode = {node | ors = refreshORNode node.ors lor}
        in FocusedList.update at newNode

listEBSDWithFocus : ArcheTree -> (EBSD -> Bool -> b) -> List b 
listEBSDWithFocus at foo = FocusedList.listWithFocus at (\a b -> foo a.ebsd b)

listORWithFocus : ArcheTree -> (OREval -> Bool -> b) -> List b 
listORWithFocus at foo = case FocusedList.getFocus at of
    Nothing   -> []
    Just ebsd -> FocusedList.listWithFocus ebsd.ors (\a b -> foo a.orEvaluation b)

getEBSDFocusKey : ArcheTree -> Maybe String
getEBSDFocusKey = FocusedList.getFocusKey

focusOnEbsd : ArcheTree -> String -> ArcheTree
focusOnEbsd = FocusedList.focusOn