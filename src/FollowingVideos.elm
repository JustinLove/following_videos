import Twitch.Deserialize exposing (User, Follow)
import Twitch.Id
import View

import Html
import Http
import Time
import Json.Decode

requestLimit = 100
requestRate = 5

type Msg
  = Self (Result Http.Error (List User))
  | Users (Result Http.Error (List User))
  | Follows (Result Http.Error (List Follow))
  | NextRequest Time.Time
  | UI (View.Msg)

type alias Model =
  { self : User
  , follows : List Follow
  , users : List User
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
  ( { self = User "-" "-"
    , follows = []
    , users = []
    , pendingUsers = []
    , pendingRequests = [fetchSelf Twitch.Id.userName]
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
          [fetchFollows [user.id]]
        }
      , Cmd.none
      )
    Self (Ok _) ->
      let _ = Debug.log "self did not find user" "" in
      (model, Cmd.none)
    Self (Err error) ->
      let _ = Debug.log "self fetch error" error in
      (model, Cmd.none)
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
      let _ = Debug.log "user fetch error" error in
      (model, Cmd.none)
    Follows (Ok follows) ->
      (
        { model
        | follows = List.append model.follows follows
        , pendingRequests = List.append model.pendingRequests
          [fetchUsers (List.map .to_id follows)]
        }
      , Cmd.none
      )
    Follows (Err error) ->
      let _ = Debug.log "follow fetch error" error in
      (model, Cmd.none)
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

fetchSelfUrl : String -> String
fetchSelfUrl name =
  "https://api.twitch.tv/helix/users?login=" ++ name

fetchSelf : String -> Cmd Msg
fetchSelf name =
  helix Self (fetchSelfUrl name) Twitch.Deserialize.users

fetchUsersUrl : List String -> String
fetchUsersUrl users =
  "https://api.twitch.tv/helix/users?id=" ++ (String.join "&id=" users)

fetchUsers : List String -> Cmd Msg
fetchUsers users =
  if List.isEmpty users then
    Cmd.none
  else
    helix Users (fetchUsersUrl users) Twitch.Deserialize.users

fetchFollowsUrl : List String -> String
fetchFollowsUrl userIds =
  "https://api.twitch.tv/helix/users/follows?first=100&from_id=" ++ (String.join "&from_id=" userIds)

fetchFollows : List String -> Cmd Msg
fetchFollows userIds =
  if List.isEmpty userIds then
    Cmd.none
  else
    helix Follows (fetchFollowsUrl userIds) Twitch.Deserialize.follows

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
