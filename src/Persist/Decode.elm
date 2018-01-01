module Persist.Decode exposing (persist, user)

import Persist exposing (Persist)
import Twitch.Deserialize exposing (User, Game)
import Uuid exposing (Uuid)

import Json.Decode exposing (..)

persist : Decoder Persist
persist =
  map2 Persist
    (field "users" (list user))
    (field "authState" (nullable Uuid.decoder))

user : Decoder User
user =
  map2 User
    (field "id" string)
    (field "displayName" string)
