module Homework exposing (..)
import Debug
import Maybe
import Url.Builder
--------------------- HOMEWORK #1

myLast : List a -> Maybe a
myLast l = case l of 
    [] -> Nothing
    [x] -> Just x
    _::xs -> myLast xs

myButLast : List a -> Maybe a
myButLast l = case l of 
    [] -> Nothing
    [x] -> Nothing
    x::xs -> if List.length xs == 1 then Just x else myButLast xs

elementAt : List a -> Int -> Maybe a
elementAt l key = 
    let
        index : List a -> Int -> Int -> Maybe a
        index list i y = case list of
            [] -> Nothing
            x::xs -> if i == y then Just x else index xs i (y + 1)
    in
        index l key 1

myLength : List a -> Int
myLength l =
    let
        count : List a -> Int -> Int
        count list index = case list of 
            [] -> index
            x::xs -> count xs (index + 1)
    in
        count l 0

myReverse : List a -> List a
myReverse l =
    let
        reverse : List a -> List a -> List a
        reverse list acc = case list of 
            [] -> acc
            x::xs -> reverse xs (x::acc)
    in
        reverse l []

compress : String -> String
compress l = 
    let
        compr : List a -> List a -> List a
        compr list acc = case list of 
            [] -> acc
            [x] -> acc
            x::s::xs -> if List.length acc == 0 then 
                            compr (s::xs) [x]
                        else if x == s then 
                            compr (s::xs) acc 
                        else 
                            compr (s::xs) (acc ++ [s])
    in
        String.fromList (compr (String.toList l) [])

dropEvery : String -> Int -> String
dropEvery source count = 
    let
        list = String.toList source
        accum l acc i = case l of 
            [] -> acc
            x::xs -> if List.length xs < i then acc else accum xs (acc++[x]) i
    in
        String.fromList (accum list [] count)

isPalindrome : List a -> Bool
isPalindrome l = List.reverse l == l

clap : String -> String
clap s = String.join " 👏 " <| String.split " " s

--------------------- HOMEWORK #2

convert : List { name : String, email : String, phone_number : String } -> List { name : String, email : String } 
convert = List.map (\el -> { name = el.name, email = el.email })

convert02 : List { name : Maybe String, email : Maybe String } -> List { name : String, email : String } 
convert02 l = 
    let 
        isEmpty : { name : Maybe String, email : Maybe String } -> Bool
        isEmpty s = s.name /= Nothing || s.email /= Nothing
        
        unpack : Maybe String -> String
        unpack el = case el of
            Just x -> x
            Nothing -> ""
    in
        List.map (\r -> { name = unpack r.name, email = unpack r.email }) (List.filter isEmpty l)

convert03 : List { name : Maybe String, email : Maybe String } -> List { name : String, email : String } 
convert03 = 
    let 
        unpack : Maybe String -> String
        unpack el = case el of
            Just x -> x
            Nothing -> "<unsepcified>"
    in
        List.map (\r -> { name = unpack r.name, email = unpack r.email })

catMaybes : List (Maybe a) -> List a
catMaybes l = 
    case l of
        x::xs -> case x of
            Nothing -> catMaybes xs
            Just v -> v :: catMaybes xs
        [] -> []

mapMaybes : (a -> Maybe b) -> List a -> List b
mapMaybes f xs = catMaybes <| List.map f xs

bird : Int
bird =
    let 
        notThree x =
            x /= 3

        incr x =
            x + 1
    in
    List.sum (List.filter notThree (List.map incr [ 1, 2, 3 ]))

-- using <|
bird2 = List.sum <| List.filter ((/=) 3) <| List.map ((+) 1) [ 1, 2, 3 ]

-- using |>
bird3 = List.map ((+) 1) [ 1, 2, 3 ] |> List.filter ((/=) 3) |> List.sum

buildStatsUrl : Int -> { startDate : Maybe String, numElems : Maybe Int } -> String
buildStatsUrl itemId ps = Url.Builder.absolute ["https://myapi.com", "api", "item", String.fromInt itemId, "stats.json"] <| (++)
        (
            case ps.startDate of 
                Nothing -> [] 
                Just x -> [Url.Builder.string "startDate" x]
        )
        (
            case ps.numElems of 
                Nothing -> []
                Just x -> [Url.Builder.int "numElems" x]
        )

type alias User = { profile : Profile }
type alias Profile = { address : Address }
type alias Address = { phone : String }

setPhone : String -> User -> User
setPhone p u = 
    let
        updateAddress a ph = {a | phone = ph}
        updateProfile pr a = {pr | address = a}
    in
        { u | profile = updateProfile u.profile <| updateAddress u.profile.address p}

--------------------- HOMEWORK #3

maybeToList : Maybe a -> List a
maybeToList v = case v of
    Just x -> [x]
    Nothing -> []

find : (a -> Bool) -> List a -> Maybe a
find f l = case List.filter f l of
    x::_ -> Just x
    [] -> Nothing

isJust : Maybe a -> Bool
isJust m = case m of
    Just _ -> True
    Nothing -> False