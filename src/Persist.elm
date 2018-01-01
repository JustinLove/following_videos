module Persist exposing (Persist)

import Twitch.Deserialize exposing (User, Game)
import Uuid exposing (Uuid)

type alias Persist =
  { users : List User
  , authState : Maybe Uuid
  }
