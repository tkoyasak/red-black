module Dict.TTTree exposing
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
    = TTNode2 (Dict k v) ( k, v ) (Dict k v)
    | TTNode3 (Dict k v) ( k, v ) (Dict k v) ( k, v ) (Dict k v)
    | TTEmpty


{-| Create an empty dictionary.
-}
empty : Dict k v
empty =
    TTEmpty


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
        TTEmpty ->
            Nothing

        TTNode2 a ( k1, v1 ) b ->
            case compare targetKey k1 of
                LT ->
                    get targetKey a

                EQ ->
                    Just v1

                GT ->
                    get targetKey b

        TTNode3 a ( k1, v1 ) b ( k2, v2 ) c ->
            case compare targetKey k1 of
                LT ->
                    get targetKey a

                EQ ->
                    Just v1

                GT ->
                    get targetKey (TTNode2 b ( k2, v2 ) c)


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
        TTEmpty ->
            n

        TTNode2 a _ b ->
            sizeHelp (sizeHelp (n + 1) b) a

        TTNode3 a _ b _ c ->
            sizeHelp (sizeHelp (sizeHelp (n + 2) c) b) a


{-| Determine if a dictionary is empty.

    isEmpty empty == True

-}
isEmpty : Dict k v -> Bool
isEmpty dict =
    case dict of
        TTEmpty ->
            True

        _ ->
            False


{-| Insert a key-value pair into a dictionary. Replaces value when there is
a collision.
-}
type InsertResult k v
    = Consumed (Dict k v)
    | Pushed (Dict k v) ( k, v ) (Dict k v)


insert : comparable -> v -> Dict comparable v -> Dict comparable v
insert key val dict =
    case insertHelp key val dict of
        Consumed a ->
            a

        Pushed a ( k1, v1 ) b ->
            TTNode2 a ( k1, v1 ) b


insertHelp : comparable -> v -> Dict comparable v -> InsertResult comparable v
insertHelp key val dict =
    case dict of
        TTEmpty ->
            Consumed (singleton key val)

        TTNode2 TTEmpty ( k1, v1 ) TTEmpty ->
            case compare key k1 of
                LT ->
                    Consumed (TTNode3 empty ( key, val ) empty ( k1, v1 ) empty)

                EQ ->
                    Consumed (TTNode2 empty ( key, val ) empty)

                GT ->
                    Consumed (TTNode3 empty ( k1, v1 ) empty ( key, val ) empty)

        TTNode3 TTEmpty ( k1, v1 ) TTEmpty ( k2, v2 ) TTEmpty ->
            case ( compare key k1, compare key k2 ) of
                ( LT, _ ) ->
                    Pushed (singleton key val) ( k1, v1 ) (singleton k2 v2)

                ( EQ, _ ) ->
                    Consumed (TTNode3 empty ( key, val ) empty ( k2, v2 ) empty)

                ( _, LT ) ->
                    Pushed (singleton k1 v1) ( key, val ) (singleton k2 v2)

                ( _, EQ ) ->
                    Consumed (TTNode3 empty ( k1, v1 ) empty ( key, val ) empty)

                ( _, GT ) ->
                    Pushed (singleton k1 v1) ( k2, v2 ) (singleton key val)

        TTNode2 a ( k1, v1 ) b ->
            case compare key k1 of
                LT ->
                    case insertHelp key val a of
                        Consumed aa ->
                            Consumed (TTNode2 aa ( k1, v1 ) b)

                        Pushed aa ( ak1, av1 ) ab ->
                            Consumed (TTNode3 aa ( ak1, av1 ) ab ( k1, v1 ) b)

                EQ ->
                    Consumed (TTNode2 a ( key, val ) b)

                GT ->
                    case insertHelp key val b of
                        Consumed ba ->
                            Consumed (TTNode2 a ( k1, v1 ) ba)

                        Pushed ba ( bk1, bv1 ) bb ->
                            Consumed (TTNode3 a ( k1, v1 ) ba ( bk1, bv1 ) bb)

        TTNode3 a ( k1, v1 ) b ( k2, v2 ) c ->
            case ( compare key k1, compare key k2 ) of
                ( LT, _ ) ->
                    case insertHelp key val a of
                        Consumed aa ->
                            Consumed (TTNode3 aa ( k1, v1 ) b ( k2, v2 ) c)

                        Pushed aa ( ak1, av1 ) ab ->
                            Pushed (TTNode2 aa ( ak1, av1 ) ab) ( k1, v1 ) (TTNode2 b ( k2, v2 ) c)

                ( EQ, _ ) ->
                    Consumed (TTNode3 a ( key, val ) b ( k2, v2 ) c)

                ( _, LT ) ->
                    case insertHelp key val b of
                        Consumed ba ->
                            Consumed (TTNode3 a ( k1, v1 ) ba ( k2, v2 ) c)

                        Pushed ba ( bk1, bv1 ) bb ->
                            Pushed (TTNode2 a ( k1, v1 ) ba) ( bk1, bv1 ) (TTNode2 bb ( k2, v2 ) c)

                ( _, EQ ) ->
                    Consumed (TTNode3 a ( k1, v1 ) b ( key, val ) c)

                ( _, GT ) ->
                    case insertHelp key val c of
                        Consumed ca ->
                            Consumed (TTNode3 a ( k1, v1 ) b ( k2, v2 ) ca)

                        Pushed ca ( ck1, cv1 ) cb ->
                            Pushed (TTNode2 a ( k1, v1 ) b) ( k2, v2 ) (TTNode2 ca ( ck1, cv1 ) cb)


{-| Remove a key-value pair from a dictionary. If the key is not found,
no changes are made.

TODO: implement

-}
type RemoveResult k v
    = Merged (Dict k v)
    | Replaced (Dict k v)
    | NotFoundKey


remove : comparable -> Dict comparable v -> Dict comparable v
remove key dict =
    case removeHelp key dict of
        Merged new ->
            new

        Replaced new ->
            new

        NotFoundKey ->
            dict


removeHelp : comparable -> Dict comparable v -> RemoveResult comparable v
removeHelp key dict =
    case dict of
        TTEmpty ->
            NotFoundKey

        TTNode2 TTEmpty ( k1, _ ) TTEmpty ->
            case compare key k1 of
                EQ ->
                    Merged empty

                _ ->
                    NotFoundKey

        TTNode3 TTEmpty ( k1, v1 ) TTEmpty ( k2, v2 ) TTEmpty ->
            case ( compare key k1, compare key k2 ) of
                ( EQ, _ ) ->
                    Replaced (singleton k2 v2)

                ( _, EQ ) ->
                    Replaced (singleton k1 v1)

                _ ->
                    NotFoundKey

        TTNode2 a ( k1, v1 ) b ->
            case compare key k1 of
                LT ->
                    case removeHelp key a of
                        NotFoundKey ->
                            NotFoundKey

                        Merged merged ->
                            case b of
                                TTEmpty ->
                                    NotFoundKey

                                TTNode2 ba ( bk1, bv1 ) bb ->
                                    Merged (TTNode3 merged ( k1, v1 ) ba ( bk1, bv1 ) bb)

                                TTNode3 ba ( bk1, bv1 ) bb ( bk2, bv2 ) bc ->
                                    Replaced (TTNode2 (TTNode2 merged ( k1, v1 ) ba) ( bk1, bv1 ) (TTNode2 bb ( bk2, bv2 ) bc))

                        Replaced replaced ->
                            Replaced (TTNode2 replaced ( k1, v1 ) b)

                EQ ->
                    case findNextLarger key b of
                        Nothing ->
                            NotFoundKey

                        Just ( keyNext, valNext ) ->
                            case removeHelp keyNext b of
                                NotFoundKey ->
                                    NotFoundKey

                                Merged merged ->
                                    case a of
                                        TTEmpty ->
                                            NotFoundKey

                                        TTNode2 aa ( ak1, av1 ) ab ->
                                            Merged (TTNode3 aa ( ak1, av1 ) ab ( keyNext, valNext ) merged)

                                        TTNode3 aa ( ak1, av1 ) ab ( ak2, av2 ) ac ->
                                            Replaced (TTNode2 (TTNode2 aa ( ak1, av1 ) ab) ( ak2, av2 ) (TTNode2 ac ( keyNext, valNext ) merged))

                                Replaced replaced ->
                                    Replaced (TTNode2 a ( keyNext, valNext ) replaced)

                GT ->
                    case removeHelp key b of
                        NotFoundKey ->
                            NotFoundKey

                        Merged merged ->
                            case a of
                                TTEmpty ->
                                    NotFoundKey

                                TTNode2 aa ( ak1, av1 ) ab ->
                                    Merged (TTNode3 aa ( ak1, av1 ) ab ( k1, v1 ) merged)

                                TTNode3 aa ( ak1, av1 ) ab ( ak2, av2 ) ac ->
                                    Replaced (TTNode2 (TTNode2 aa ( ak1, av1 ) ab) ( ak2, av2 ) (TTNode2 ac ( k1, v1 ) merged))

                        Replaced replaced ->
                            Replaced (TTNode2 a ( k1, v1 ) replaced)

        TTNode3 a ( k1, v1 ) b ( k2, v2 ) c ->
            case ( compare key k1, compare key k2 ) of
                ( LT, _ ) ->
                    case removeHelp key a of
                        NotFoundKey ->
                            NotFoundKey

                        Merged merged ->
                            case b of
                                TTEmpty ->
                                    NotFoundKey

                                TTNode2 ba ( bk1, bv1 ) bb ->
                                    case c of
                                        TTEmpty ->
                                            NotFoundKey

                                        TTNode2 ca ( ck1, cv1 ) cb ->
                                            Replaced (TTNode2 (TTNode2 merged ( k1, v1 ) ba) ( bk1, bv1 ) (TTNode3 bb ( k2, v2 ) ca ( ck1, cv1 ) cb))

                                        TTNode3 ca ( ck1, cv1 ) cb ( ck2, cv2 ) cc ->
                                            Replaced (TTNode3 (TTNode2 merged ( k1, v1 ) ba) ( bk1, bv1 ) (TTNode2 bb ( k2, v2 ) ca) ( ck1, cv1 ) (TTNode2 cb ( ck2, cv2 ) cc))

                                TTNode3 ba ( bk1, bv1 ) bb ( bk2, bv2 ) bc ->
                                    Replaced (TTNode3 (TTNode2 merged ( k1, v1 ) ba) ( bk1, bv1 ) (TTNode2 bb ( bk2, bv2 ) bc) ( k2, v2 ) c)

                        Replaced replaced ->
                            Replaced (TTNode3 replaced ( k1, v1 ) b ( k2, v2 ) c)

                ( EQ, _ ) ->
                    case findNextLarger key b of
                        Nothing ->
                            NotFoundKey

                        Just ( keyNext, valNext ) ->
                            case removeHelp keyNext b of
                                NotFoundKey ->
                                    NotFoundKey

                                Merged merged ->
                                    case a of
                                        TTEmpty ->
                                            NotFoundKey

                                        TTNode2 aa ( ak1, av1 ) ab ->
                                            case c of
                                                TTEmpty ->
                                                    NotFoundKey

                                                TTNode2 _ _ _ ->
                                                    Replaced (TTNode2 (TTNode3 aa ( ak1, av1 ) ab ( keyNext, valNext ) merged) ( k2, v2 ) c)

                                                TTNode3 ca ( ck1, cv1 ) cb ( ck2, cv2 ) cc ->
                                                    Replaced (TTNode3 a ( keyNext, valNext ) (TTNode2 merged ( k2, v2 ) ca) ( ck1, cv1 ) (TTNode2 cb ( ck2, cv2 ) cc))

                                        TTNode3 aa ( ak1, av1 ) ab ( ak2, av2 ) ac ->
                                            Replaced (TTNode3 (TTNode2 aa ( ak1, av1 ) ab) ( ak2, av2 ) (TTNode2 ac ( k1, v1 ) merged) ( k2, v2 ) c)

                                Replaced replaced ->
                                    Replaced (TTNode3 a ( keyNext, valNext ) replaced ( k2, v2 ) c)

                ( _, LT ) ->
                    case removeHelp key b of
                        NotFoundKey ->
                            NotFoundKey

                        Merged merged ->
                            case a of
                                TTEmpty ->
                                    NotFoundKey

                                TTNode2 aa ( ak1, av1 ) ab ->
                                    case c of
                                        TTEmpty ->
                                            NotFoundKey

                                        TTNode2 _ _ _ ->
                                            Replaced (TTNode2 (TTNode3 aa ( ak1, av1 ) ab ( k1, v1 ) merged) ( k2, v2 ) c)

                                        TTNode3 ca ( ck1, cv1 ) cb ( ck2, cv2 ) cc ->
                                            Replaced (TTNode3 a ( k1, v1 ) (TTNode2 merged ( k2, v2 ) ca) ( ck1, cv1 ) (TTNode2 cb ( ck2, cv2 ) cc))

                                TTNode3 aa ( ak1, av1 ) ab ( ak2, av2 ) ac ->
                                    Replaced (TTNode3 (TTNode2 aa ( ak1, av1 ) ab) ( ak2, av2 ) (TTNode2 ac ( k1, v1 ) merged) ( k2, v2 ) c)

                        Replaced replaced ->
                            Replaced (TTNode3 a ( k1, v1 ) replaced ( k2, v2 ) c)

                ( _, EQ ) ->
                    case findNextLarger key c of
                        Nothing ->
                            NotFoundKey

                        Just ( keyNext, valNext ) ->
                            case removeHelp keyNext c of
                                NotFoundKey ->
                                    NotFoundKey

                                Merged merged ->
                                    case b of
                                        TTEmpty ->
                                            NotFoundKey

                                        TTNode2 ba ( bk1, bv1 ) bb ->
                                            case a of
                                                TTEmpty ->
                                                    NotFoundKey

                                                TTNode2 aa ( ak1, av1 ) ab ->
                                                    Replaced (TTNode2 (TTNode3 aa ( ak1, av1 ) ab ( k1, v1 ) ba) ( bk1, bv1 ) (TTNode2 bb ( keyNext, valNext ) merged))

                                                TTNode3 aa ( ak1, av1 ) ab ( ak2, av2 ) ac ->
                                                    Replaced (TTNode3 (TTNode2 aa ( ak1, av1 ) ab) ( ak2, av2 ) (TTNode2 ac ( k1, v1 ) ba) ( bk1, bv1 ) (TTNode2 bb ( keyNext, valNext ) merged))

                                        TTNode3 ba ( bk1, bv1 ) bb ( bk2, bv2 ) bc ->
                                            Replaced (TTNode3 a ( k1, v1 ) (TTNode2 ba ( bk1, bv1 ) bb) ( bk2, bv2 ) (TTNode2 bc ( keyNext, valNext ) merged))

                                Replaced replaced ->
                                    Replaced (TTNode3 a ( k1, v1 ) b ( keyNext, valNext ) replaced)

                ( _, GT ) ->
                    case removeHelp key c of
                        NotFoundKey ->
                            NotFoundKey

                        Merged merged ->
                            case b of
                                TTEmpty ->
                                    NotFoundKey

                                TTNode2 ba ( bk1, bv1 ) bb ->
                                    case a of
                                        TTEmpty ->
                                            NotFoundKey

                                        TTNode2 aa ( ak1, av1 ) ab ->
                                            Replaced (TTNode2 (TTNode3 aa ( ak1, av1 ) ab ( k1, v1 ) ba) ( bk1, bv1 ) (TTNode2 bb ( k2, v2 ) merged))

                                        TTNode3 aa ( ak1, av1 ) ab ( ak2, av2 ) ac ->
                                            Replaced (TTNode3 (TTNode2 aa ( ak1, av1 ) ab) ( ak2, av2 ) (TTNode2 ac ( k1, v1 ) ba) ( bk1, bv1 ) (TTNode2 bb ( k2, v2 ) merged))

                                TTNode3 ba ( bk1, bv1 ) bb ( bk2, bv2 ) bc ->
                                    Replaced (TTNode3 a ( k1, v1 ) (TTNode2 ba ( bk1, bv1 ) bb) ( bk2, bv2 ) (TTNode2 bc ( k2, v2 ) merged))

                        Replaced replaced ->
                            Replaced (TTNode3 a ( k1, v1 ) b ( k2, v2 ) replaced)


findNextLarger : comparable -> Dict comparable v -> Maybe ( comparable, v )
findNextLarger key dict =
    case dict of
        TTEmpty ->
            Nothing

        TTNode2 TTEmpty ( k1, v1 ) _ ->
            case compare key k1 of
                LT ->
                    Just ( k1, v1 )

                _ ->
                    Nothing

        TTNode3 TTEmpty ( k1, v1 ) _ _ _ ->
            case compare key k1 of
                LT ->
                    Just ( k1, v1 )

                _ ->
                    Nothing

        TTNode2 a _ _ ->
            findNextLarger key a

        TTNode3 a _ _ _ _ ->
            findNextLarger key a


{-| Update the value of a dictionary for a specific key with a given function.
-}
update : comparable -> (Maybe v -> Maybe v) -> Dict comparable v -> Dict comparable v
update key alter dict =
    case alter (get key dict) of
        Just val ->
            insert key val dict

        Nothing ->
            remove key dict


{-| Create a dictionary with one key-value pair.
-}
singleton : comparable -> v -> Dict comparable v
singleton key val =
    TTNode2 empty ( key, val ) empty



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
    let
        stepState : comparable -> b -> ( List ( comparable, a ), result ) -> ( List ( comparable, a ), result )
        stepState rKey rVal ( list, result ) =
            case list of
                [] ->
                    ( list, rightStep rKey rVal result )

                ( lKey, lVal ) :: rest ->
                    case compare lKey rKey of
                        LT ->
                            stepState rKey rVal ( rest, leftStep lKey lVal result )

                        EQ ->
                            ( rest, bothStep lKey lVal rVal result )

                        GT ->
                            ( list, rightStep rKey rVal result )

        ( leftovers, intermediateResult ) =
            foldl stepState ( toList leftDict, initialResult ) rightDict
    in
    List.foldl (\( k, v ) result -> leftStep k v result) intermediateResult leftovers



-- TRANSFORM


{-| Apply a function to all values in a dictionary.
-}
map : (k -> a -> b) -> Dict k a -> Dict k b
map func dict =
    case dict of
        TTEmpty ->
            TTEmpty

        TTNode2 a ( k1, v1 ) b ->
            TTNode2 (map func a) ( k1, func k1 v1 ) (map func b)

        TTNode3 a ( k1, v1 ) b ( k2, v2 ) c ->
            TTNode3 (map func a) ( k1, func k1 v1 ) (map func b) ( k2, func k2 v2 ) (map func c)


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
        TTEmpty ->
            acc

        TTNode2 a ( k1, v1 ) b ->
            foldl func (func k1 v1 (foldl func acc a)) b

        TTNode3 a ( k1, v1 ) b ( k2, v2 ) c ->
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
        TTEmpty ->
            acc

        TTNode2 a ( k1, v1 ) b ->
            foldr func (func k1 v1 (foldr func acc b)) a

        TTNode3 a ( k1, v1 ) b ( k2, v2 ) c ->
            foldr func (func k1 v1 (foldr func (func k2 v2 (foldr func acc c)) b)) a


{-| Keep only the key-value pairs that pass the given test.
-}
filter : (comparable -> v -> Bool) -> Dict comparable v -> Dict comparable v
filter isGood dict =
    let
        func : comparable -> v -> Dict comparable v -> Dict comparable v
        func k v d =
            if isGood k v then
                insert k v d

            else
                d
    in
    foldl func empty dict


{-| Partition a dictionary according to some test. The first dictionary
contains all key-value pairs which passed the test, and the second contains
the pairs that did not.
-}
partition : (comparable -> v -> Bool) -> Dict comparable v -> ( Dict comparable v, Dict comparable v )
partition isGood dict =
    let
        func : comparable -> v -> ( Dict comparable v, Dict comparable v ) -> ( Dict comparable v, Dict comparable v )
        func k v ( t1, t2 ) =
            if isGood k v then
                ( insert k v t1, t2 )

            else
                ( t1, insert k v t2 )
    in
    foldl func ( empty, empty ) dict



-- LISTS


{-| Get all of the keys in a dictionary, sorted from lowest to highest.

    keys (fromList [ ( 0, "Alice" ), ( 1, "Bob" ) ]) == [ 0, 1 ]

-}
keys : Dict k v -> List k
keys dict =
    foldr (\key _ list -> key :: list) [] dict


{-| Get all of the values in a dictionary, in the order of their keys.

    values (fromList [ ( 0, "Alice" ), ( 1, "Bob" ) ]) == [ "Alice", "Bob" ]

-}
values : Dict k v -> List v
values dict =
    foldr (\_ val list -> val :: list) [] dict


{-| Convert a dictionary into an association list of key-value pairs, sorted by keys.
-}
toList : Dict k v -> List ( k, v )
toList dict =
    foldr (\key val list -> ( key, val ) :: list) [] dict


{-| Convert an association list into a dictionary.
-}
fromList : List ( comparable, v ) -> Dict comparable v
fromList assocs =
    List.foldl (\( key, val ) dict -> insert key val dict) empty assocs
