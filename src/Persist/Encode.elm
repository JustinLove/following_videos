module Persist.Encode exposing (persist, user)

import Persist exposing (Persist, User)
import Uuid exposing (Uuid)

import Json.Encode exposing (..)

persist : Persist -> Value
persist p =
  object
    [ ("users", list user p.users)
    , ("authState", maybe Uuid.encode p.authState)
    ]

user : User -> Value
user u =
  object
    [ ("id", string u.id)
    , ("displayName", string u.displayName)
    ]

maybe : (a -> Value) -> Maybe a -> Value
maybe encode m =
  case m of
    Just v -> encode v
    Nothing -> null
