module Dict.BBTree exposing
    ( Dict(..)
    , empty, singleton, insert, update, remove
    , isEmpty, member, get, size
    , keys, values, toList, fromList
    , map, foldl, foldr, filter, partition
    , union, intersect, diff, merge
    )

{-| A dictionary mapping unique keys to values. The keys can be any comparable
type. This includes `Int`, `Float`, `Time`, `Char`, `String`, and tuples or
lists of comparable types.

Insert, remove, and query operations all take _O(log n)_ time.


# Dictionaries

@docs Dict


# Build

@docs empty, singleton, insert, update, remove


# Query

@docs isEmpty, member, get, size


# Lists

@docs keys, values, toList, fromList


# Transform

@docs map, foldl, foldr, filter, partition


# Combine

@docs union, intersect, diff, merge

-}

import Basics exposing (..)
import List exposing (..)
import Maybe exposing (..)



-- DICTIONARIES


{-| A dictionary of keys and values. So a `Dict String User` is a dictionary
that lets you look up a `String` (such as user names) and find the associated
`User`.

    import Dict exposing (Dict)

    users : Dict String User
    users =
        Dict.fromList
            [ ( "Alice", User "Alice" 28 1.65 )
            , ( "Bob", User "Bob" 19 1.82 )
            , ( "Chuck", User "Chuck" 33 1.75 )
            ]

    type alias User =
        { name : String
        , age : Int
        , height : Float
        }

-}
type Dict k v
    = BBNode1 (Dict k v) ( k, v ) (Dict k v)
    | BBNode2 (Dict k v) ( k, v ) (Dict k v) ( k, v ) (Dict k v)
    | BBEmpty


{-| Create an empty dictionary.
-}
empty : Dict k v
empty =
    BBEmpty


{-| Get the value associated with a key. If the key is not found, return
`Nothing`. This is useful when you are not sure if a key will be in the
dictionary.

    animals = fromList [ ("Tom", Cat), ("Jerry", Mouse) ]

    get "Tom"   animals == Just Cat
    get "Jerry" animals == Just Mouse
    get "Spike" animals == Nothing

-}
get : comparable -> Dict comparable v -> Maybe v
get targetKey dict =
    case dict of
        BBEmpty ->
            Nothing

        BBNode1 a ( k1, v1 ) b ->
            case compare targetKey k1 of
                LT ->
                    get targetKey a

                EQ ->
                    Just v1

                GT ->
                    get targetKey b

        BBNode2 a ( k1, v1 ) b ( k2, v2 ) c ->
            case compare targetKey k1 of
                LT ->
                    get targetKey a

                EQ ->
                    Just v1

                GT ->
                    case compare targetKey k2 of
                        LT ->
                            get targetKey b

                        EQ ->
                            Just v2

                        GT ->
                            get targetKey c


{-| Determine if a key is in a dictionary.
-}
member : comparable -> Dict comparable v -> Bool
member key dict =
    case get key dict of
        Just _ ->
            True

        Nothing ->
            False


{-| Determine the number of key-value pairs in the dictionary.
-}
size : Dict k v -> Int
size dict =
    sizeHelp 0 dict


sizeHelp : Int -> Dict k v -> Int
sizeHelp n dict =
    case dict of
        BBEmpty ->
            n

        BBNode1 a _ b ->
            sizeHelp (sizeHelp (n + 1) b) a

        BBNode2 a _ b _ c ->
            sizeHelp (sizeHelp (sizeHelp (n + 2) c) b) a


{-| Determine if a dictionary is empty.

    isEmpty empty == True

-}
isEmpty : Dict k v -> Bool
isEmpty dict =
    case dict of
        BBEmpty ->
            True

        _ ->
            False


{-| Insert a key-value pair into a dictionary. Replaces value when there is
a collision.

TODO: implement

-}
insert : comparable -> v -> Dict comparable v -> Dict comparable v
insert key value dict =
   empty


{-| Remove a key-value pair from a dictionary. If the key is not found,
no changes are made.

TODO: implement

-}
remove : comparable -> Dict comparable v -> Dict comparable v
remove key dict =
    empty


{-| Update the value of a dictionary for a specific key with a given function.

TODO: implement

-}
update : comparable -> (Maybe v -> Maybe v) -> Dict comparable v -> Dict comparable v
update targetKey alter dictionary =
    empty

{-| Create a dictionary with one key-value pair.
-}
singleton : comparable -> v -> Dict comparable v
singleton key value =
    BBNode1 BBEmpty ( key, value ) BBEmpty



-- COMBINE


{-| Combine two dictionaries. If there is a collision, preference is given
to the first dictionary.
-}
union : Dict comparable v -> Dict comparable v -> Dict comparable v
union t1 t2 =
    foldl insert t2 t1


{-| Keep a key-value pair when its key appears in the second dictionary.
Preference is given to values in the first dictionary.
-}
intersect : Dict comparable v -> Dict comparable v -> Dict comparable v
intersect t1 t2 =
    filter (\k _ -> member k t2) t1


{-| Keep a key-value pair when its key does not appear in the second dictionary.
-}
diff : Dict comparable a -> Dict comparable b -> Dict comparable a
diff t1 t2 =
    foldl (\k v t -> remove k t) t1 t2


{-| The most general way of combining two dictionaries. You provide three
accumulators for when a given key appears:

1.  Only in the left dictionary.
2.  In both dictionaries.
3.  Only in the right dictionary.

You then traverse all the keys from lowest to highest, building up whatever
you want.

TODO: implement

-}
merge :
    (comparable -> a -> result -> result)
    -> (comparable -> a -> b -> result -> result)
    -> (comparable -> b -> result -> result)
    -> Dict comparable a
    -> Dict comparable b
    -> result
    -> result
merge leftStep bothStep rightStep leftDict rightDict initialResult =
    initialResult



-- TRANSFORM


{-| Apply a function to all values in a dictionary.
-}
map : (k -> a -> b) -> Dict k a -> Dict k b
map func dict =
    case dict of
        BBEmpty ->
            BBEmpty

        BBNode1 a ( k1, v1 ) b ->
            BBNode1 (map func a) ( k1, func k1 v1 ) (map func b)

        BBNode2 a ( k1, v1 ) b ( k2, v2 ) c ->
            BBNode2 (map func a) ( k1, func k1 v1 ) (map func b) ( k2, func k2 v2 ) (map func c)


{-| Fold over the key-value pairs in a dictionary from lowest key to highest key.

    import Dict exposing (Dict)

    getAges : Dict String User -> List String
    getAges users =
        Dict.foldl addAge [] users

    addAge : String -> User -> List String -> List String
    addAge _ user ages =
        user.age :: ages

    -- getAges users == [33,19,28]

-}
foldl : (k -> v -> b -> b) -> b -> Dict k v -> b
foldl func acc dict =
    case dict of
        BBEmpty ->
            acc

        BBNode1 a ( k1, v1 ) b ->
            foldl func (func k1 v1 (foldl func acc a)) b

        BBNode2 a ( k1, v1 ) b ( k2, v2 ) c ->
            foldl func (func k2 v2 (foldl func (func k1 v1 (foldl func acc a)) b)) c


{-| Fold over the key-value pairs in a dictionary from highest key to lowest key.

    import Dict exposing (Dict)

    getAges : Dict String User -> List String
    getAges users =
        Dict.foldr addAge [] users

    addAge : String -> User -> List String -> List String
    addAge _ user ages =
        user.age :: ages

    -- getAges users == [28,19,33]

-}
foldr : (k -> v -> b -> b) -> b -> Dict k v -> b
foldr func acc dict =
    case dict of
        BBEmpty ->
            acc

        BBNode1 a ( k1, v1 ) b ->
            foldr func (func k1 v1 (foldr func acc b)) a

        BBNode2 a ( k1, v1 ) b ( k2, v2 ) c ->
            foldr func (func k1 v1 (foldr func (func k2 v2 (foldr func acc c)) b)) a


{-| Keep only the key-value pairs that pass the given test.
-}
filter : (comparable -> v -> Bool) -> Dict comparable v -> Dict comparable v
filter isGood dict =
    let
        filt : comparable -> v -> Dict comparable v -> Dict comparable v
        filt k v d =
            if isGood k v then
                insert k v d

            else
                d
    in
    foldl filt empty dict


{-| Partition a dictionary according to some test. The first dictionary
contains all key-value pairs which passed the test, and the second contains
the pairs that did not.
-}
partition : (comparable -> v -> Bool) -> Dict comparable v -> ( Dict comparable v, Dict comparable v )
partition isGood dict =
    let
        add : comparable -> v -> ( Dict comparable v, Dict comparable v ) -> ( Dict comparable v, Dict comparable v )
        add k v ( t1, t2 ) =
            if isGood k v then
                ( insert k v t1, t2 )

            else
                ( t1, insert k v t2 )
    in
    foldl add ( empty, empty ) dict



-- LISTS


{-| Get all of the keys in a dictionary, sorted from lowest to highest.

    keys (fromList [ ( 0, "Alice" ), ( 1, "Bob" ) ]) == [ 0, 1 ]

-}
keys : Dict k v -> List k
keys dict =
    foldr (\key value keyList -> key :: keyList) [] dict


{-| Get all of the values in a dictionary, in the order of their keys.

    values (fromList [ ( 0, "Alice" ), ( 1, "Bob" ) ]) == [ "Alice", "Bob" ]

-}
values : Dict k v -> List v
values dict =
    foldr (\key value valueList -> value :: valueList) [] dict


{-| Convert a dictionary into an association list of key-value pairs, sorted by keys.
-}
toList : Dict k v -> List ( k, v )
toList dict =
    foldr (\key value list -> ( key, value ) :: list) [] dict


{-| Convert an association list into a dictionary.
-}
fromList : List ( comparable, v ) -> Dict comparable v
fromList assocs =
    List.foldl (\( key, value ) dict -> insert key value dict) empty assocs
