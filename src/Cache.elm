module Cache (Cache, Update, create, Reference, reference) where

{-| This module makes it possible to integrate Elm with real-time storage services like Firebase or Parse.
The typical usage is:
1. Send cache update commands from your JavaScript glue code.
1. Define a cache for each entity type in your model which is stored in the cloud with `create`.
1. When referencing a remote object, store a `Reference` and resolve it lazily with its `lookup` function.
1. Collect the URLs of all remote objects for each type, and send them to your JavaScript code to observe them.

# Defining a cache
@docs Cache, Update, create

# Referring values in a cache
@docs Reference, reference

-}

import Dict (..)
import Dict
import Signal (..)
import Json.Decode (..)
import Json.Decode as Decode

{-| Associates values to keys.
Its main use case is caching remote objects by their URL.

    writerCache : Signal (Cache Writer)
-}
type alias Cache a = Dict String a

{-| A cache update command.
Indicates that a new value has to be inserted to the cache or an existing value has to be removed from it.

    port writerFeed : Signal (Cache.Update Writer)
-}
type alias Update a = Maybe {
  url: String,
  value: Maybe a
}

{-| Maintains the state of a cache by processing an update feed.
The given function is used to decode raw values coming from the update feed to objects stored in the cache.

    writerCache = writerFeed |> Cache.create identity
-}
create : (a -> b) -> Signal (Update a) -> Signal (Cache b)
create transform feed = feed |> foldp (update transform) empty

update : (a -> b) -> Update a -> Cache b -> Cache b
update transform entry cache =
  case entry of
    Nothing -> cache
    Just { url, value } ->
      case value of 
        Just realValue -> cache |> insert url (realValue |> transform)
        Nothing -> cache |> remove url

{-| Represents a type-safe remote reference.
Use this type in your model whenever you cross-reference objects stored in a cache.

    type alias Book = {
      title: String,
      authors: List (Reference Writer)
    }
-}
type alias Reference a = {
  url: String,
  lookup: Cache a -> Maybe a
}

{-| Decodes a JavaScript string to a remote reference.

    bookDecoder =
      object2 Book
        ("title" := string)
        ("authors" := list reference)
-}
reference : Decoder (Reference a)
reference =
  string |> Decode.map (\url ->
    {
      url = url,
      lookup = Dict.get url
    }
  )