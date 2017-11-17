import Twitch.Deserialize exposing (User)
import Twitch.Id
import View

import Html
import Http
import Time
import Json.Decode

requestLimit = 100
requestRate = 5

type Msg
  = Users (Result Http.Error (List User))
  | Self (Result Http.Error (List User))
  | NextRequest Time.Time
  | UI (View.Msg)

type alias Model =
  { users : List User
  , self : User
  , pendingUsers : List String
  , pendingRequests : List (Cmd Msg)
  , outstandingRequests : Int
  }

main = Html.program
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = (\model -> Html.map UI (View.view model))
  }

init : (Model, Cmd Msg)
init =
  ( fetchNextUserBatch requestLimit
    { users = []
    , self = User "-" "-"
    , pendingUsers = []
    , pendingRequests = []
    , outstandingRequests = 0
    }
  , Cmd.none
  )

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Self (Ok (user::_)) ->
      ( { model
        | self = user
        , pendingRequests = List.append model.pendingRequests
          []
        }
      , Cmd.none
      )
    Self (Ok _) ->
      { e = Debug.log "self did not find user" ""
      , r = (model, Cmd.none)}.r
    Self (Err error) ->
      { e = Debug.log "self fetch error" error
      , r = (model, Cmd.none)}.r
    Users (Ok users) ->
      (fetchNextUserBatch requestLimit
        { model
        | users = List.append model.users users
        , pendingRequests = List.append model.pendingRequests
          []
        }
      , Cmd.none
      )
    Users (Err error) ->
      { e = Debug.log "user fetch error" error
      , r = (model, Cmd.none)}.r
    NextRequest _ ->
      case model.pendingRequests of
        next :: rest ->
          ( { model
            | pendingRequests = rest
            , outstandingRequests = model.outstandingRequests + (if next == Cmd.none then 0 else 1)
            }, next)
        _ -> (model, Cmd.none)
    UI (View.None) ->
      (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  if List.isEmpty model.pendingRequests then
    Sub.none
  else
    Time.every (Time.second/requestRate) NextRequest

fetchNextUserBatch : Int -> Model -> Model
fetchNextUserBatch batch model =
  { model
  | pendingUsers = List.drop batch model.pendingUsers
  , pendingRequests = List.append model.pendingRequests
    [fetchUsers <| List.take batch model.pendingUsers]
  }

fetchUsersUrl : List String -> String
fetchUsersUrl users =
  "https://api.twitch.tv/helix/users?login=" ++ (String.join "&login=" users)

fetchUsers : List String -> Cmd Msg
fetchUsers users =
  if List.isEmpty users then
    Cmd.none
  else
    helix Users (fetchUsersUrl users) Twitch.Deserialize.users

helix : ((Result Http.Error a) -> Msg) -> String -> Json.Decode.Decoder a -> Cmd Msg
helix tagger url decoder =
  Http.send tagger <| Http.request
    { method = "GET"
    , headers =
      [ Http.header "Client-ID" Twitch.Id.clientId
      ]
    , url = url
    , body = Http.emptyBody
    , expect = Http.expectJson decoder
    , timeout = Nothing
    , withCredentials = False
    }
