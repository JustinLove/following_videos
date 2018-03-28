module Persist exposing (Persist, User)

import Uuid exposing (Uuid)

type alias Persist =
  { users : List User
  , authState : Maybe Uuid
  }

type alias User =
  { id : String
  , displayName : String
  }
