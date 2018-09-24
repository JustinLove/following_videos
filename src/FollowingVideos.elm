module FollowingVideos exposing (..)

import Persist exposing (Persist, User)
import Persist.Encode
import Persist.Decode
import Twitch.Helix.Decode as Helix exposing (Follow, Video)
import Twitch.Helix as Helix
import TwitchId
import View
import Harbor

import Browser
import Http
import Time exposing (Posix, Zone)
import Json.Decode
import Json.Encode
import Uuid exposing (Uuid)
import Random
import Set exposing (Set)
import Task
import Url exposing (Url)
import Url.Parser
import Url.Parser.Query

requestLimit = 100
rateLimit = 30
authRateLimit = 120
videoLimit = requestLimit

type Msg
  = Loaded (Maybe Persist)
  | CurrentZone Zone
  | Self (Result Http.Error (List Helix.User))
  | Follows (Result Http.Error (List Follow))
  | Users (Result Http.Error (List Helix.User))
  | Videos (Result Http.Error (List Video))
  | NextRequest Posix
  | AuthState Uuid
  | UI (View.Msg)

type alias Model =
  { location : Url
  , zone : Zone
  , responseState : Maybe Uuid
  , requestState : Maybe Uuid
  , auth : Maybe String
  , self : User
  , follows : List Follow
  , users : List User
  , videos : List Video
  , pendingUsers : List String
  , pendingRequests : List (Cmd Msg)
  , outstandingRequests : Int
  }

main = Browser.document
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = View.document UI
  }

init : String -> (Model, Cmd Msg)
init href =
  let
    url = Url.fromString href
      |> Maybe.withDefault (Url Url.Http "" Nothing "" Nothing Nothing)
    auth = extractHashArgument "access_token" url
    state = extractHashArgument "state" url
      |> Maybe.andThen Uuid.fromString
  in
  ( { location = url
    , zone = Time.utc
    , responseState = state
    , requestState = Nothing
    , auth = auth
    , self = User "-" "-"
    , follows = []
    , users = []
    , videos = []
    , pendingUsers = []
    , pendingRequests = []
    , outstandingRequests = 0
    }
  , Cmd.batch
    [ Random.generate AuthState Uuid.uuidGenerator
    , Task.perform CurrentZone Time.here
    ]
  )

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Loaded mstate ->
      ( ( case mstate of
          Just state ->
            resolveLoaded state model
          Nothing ->
            model
        )
      , Cmd.none
      )
    CurrentZone zone ->
      ( {model | zone = zone}, Cmd.none)
    Self (Ok (user::_)) ->
      ( { model
        | self = importUser user
        , pendingRequests = List.append model.pendingRequests
          [fetchFollows model.auth [user.id]]
        }
      , Cmd.none
      )
    Self (Ok _) ->
      let _ = Debug.log "self did not find user" "" in
      (model, Cmd.none)
    Self (Err error) ->
      let _ = Debug.log "self fetch error" error in
      (model, Cmd.none)
    Follows (Ok follows) ->
      let userIds = List.map .to_id follows in
      (
        { model
        | follows = List.append model.follows follows
        , pendingUsers = missingUsers userIds model
        , pendingRequests = List.append model.pendingRequests
          (List.take videoLimit <| List.map (fetchVideos model.auth) userIds)
        }
        |> fetchNextUserBatch requestLimit
      , Cmd.none
      )
    Follows (Err error) ->
      let _ = Debug.log "follow fetch error" error in
      (model, Cmd.none)
    Users (Ok users) ->
      { model
      | users = List.append model.users <| List.map importUser users
      }
      |> fetchNextUserBatch requestLimit
      |> persist
    Users (Err error) ->
      let _ = Debug.log "user fetch error" error in
      (model, Cmd.none)
    Videos (Ok videos) ->
      ( { model
        | videos = List.append model.videos videos
        }
      , Cmd.none
      )
    Videos (Err error) ->
      let _ = Debug.log "video fetch error" error in
      (model, Cmd.none)
    NextRequest _ ->
      case model.pendingRequests of
        next :: rest ->
          ( { model
            | pendingRequests = rest
            , outstandingRequests = model.outstandingRequests + (if next == Cmd.none then 0 else 1)
            }, next)
        _ -> (model, Cmd.none)
    AuthState uuid ->
      {model | requestState = Just uuid }
        |> persist
    UI (View.Refresh) ->
      ( { model
        | videos = []
        , pendingRequests = List.append model.pendingRequests
          (model.users
            |> List.map .id
            |> List.map (fetchVideos model.auth)
            |> List.take videoLimit
          )
        }
      , Cmd.none)

resolveLoaded : Persist -> Model -> Model
resolveLoaded state model =
  if model.responseState == state.authState then
    { model
    | users = state.users
    , pendingRequests = case model.auth of
      Just _ ->
        [ fetchSelf model.auth ]
      Nothing ->
        []
    }
  else
    let _ = Debug.log "auth state mismatch" [model.responseState, state.authState] in
    { model
    | users = state.users
    , auth = Nothing
    }

missingUsers : List String -> Model -> List String
missingUsers userIds model =
  let
    known = Set.fromList <| List.map (.id >> String.toLower) model.users
  in
    Set.toList <| Set.diff (Set.fromList userIds) known

fetchNextUserBatch : Int -> Model -> Model
fetchNextUserBatch batch model =
  { model
  | pendingUsers = List.drop batch model.pendingUsers
  , pendingRequests = List.append
      [fetchUsers model.auth <| List.take batch model.pendingUsers]
      model.pendingRequests
  }

persist : Model -> (Model, Cmd Msg)
persist model =
  (model, saveModel model)

saveModel : Model -> Cmd Msg
saveModel model =
  Persist model.users model.requestState
    |> Persist.Encode.persist
    |> Json.Encode.encode 0
    |> Harbor.save

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ if List.isEmpty model.pendingRequests then
        Sub.none
      else
        Time.every ((requestRate model.auth)*1.05) NextRequest
    , Harbor.loaded receiveLoaded
    ]

receiveLoaded : Maybe String -> Msg
receiveLoaded mstring =
  mstring
    |> Maybe.andThen (\string ->
      string
       |> Json.Decode.decodeString Persist.Decode.persist
       |> Result.mapError (Debug.log "persist decode error")
       |> Result.toMaybe
      )
    |> Loaded

requestRate : Maybe String -> Float
requestRate auth =
  case auth of
    Just _ ->
      (60*1000/authRateLimit)
    Nothing ->
      (60*1000/rateLimit)

importUser : Helix.User -> User
importUser user =
  { id = user.id
  , displayName = user.displayName
  }

fetchSelfUrl : String
fetchSelfUrl =
  "https://api.twitch.tv/helix/users"

fetchSelf : Maybe String -> Cmd Msg
fetchSelf auth =
  Helix.send <|
    { clientId = TwitchId.clientId
    , auth = auth
    , decoder = Helix.users
    , tagger = Self
    , url = fetchSelfUrl
    }

fetchUsersUrl : List String -> String
fetchUsersUrl users =
  "https://api.twitch.tv/helix/users?id=" ++ (String.join "&id=" users)

fetchUsers : Maybe String -> List String -> Cmd Msg
fetchUsers auth users =
  if List.isEmpty users then
    Cmd.none
  else
    Helix.send <|
      { clientId = TwitchId.clientId
      , auth = auth
      , decoder = Helix.users
      , tagger = Users
      , url = (fetchUsersUrl users)
      }

fetchFollowsUrl : List String -> String
fetchFollowsUrl userIds =
  "https://api.twitch.tv/helix/users/follows?first=100&from_id=" ++ (String.join "&from_id=" userIds)

fetchFollows : Maybe String -> List String -> Cmd Msg
fetchFollows auth userIds =
  if List.isEmpty userIds then
    Cmd.none
  else
    Helix.send <|
      { clientId = TwitchId.clientId
      , auth = auth
      , decoder = Helix.follows
      , tagger = Follows
      , url = (fetchFollowsUrl userIds)
      }

fetchVideosUrl : String -> String
fetchVideosUrl userId =
  "https://api.twitch.tv/helix/videos?first=3&period=week&user_id=" ++ userId

fetchVideos : Maybe String -> String -> Cmd Msg
fetchVideos auth userId =
  Helix.send <|
    { clientId = TwitchId.clientId
    , auth = auth
    , decoder = Helix.videos
    , tagger = Videos
    , url = (fetchVideosUrl userId)
    }

extractHashArgument : String -> Url -> Maybe String
extractHashArgument key location =
  { location | path = "", query = location.fragment }
    |> Url.Parser.parse (Url.Parser.query (Url.Parser.Query.string key))
    |> Maybe.withDefault Nothing
