module FollowingVideos exposing (..)

import Persist exposing (Persist)
import Persist.Encode
import Persist.Decode
import Twitch.Deserialize exposing (User, Follow, Video)
import Twitch.Id
import View
import Harbor

import Html
import Navigation exposing (Location)
import Http
import Time
import Json.Decode
import Json.Encode
import Uuid exposing (Uuid)
import Random.Pcg as Random
import Set exposing (Set)

requestLimit = 100
rateLimit = 30
authRateLimit = 120
videoLimit = requestLimit

type Msg
  = Loaded (Maybe Persist)
  | CurrentUrl Location
  | Self (Result Http.Error (List User))
  | Follows (Result Http.Error (List Follow))
  | Users (Result Http.Error (List User))
  | Videos (Result Http.Error (List Video))
  | NextRequest Time.Time
  | AuthState Uuid
  | UI (View.Msg)

type alias Model =
  { location : Location
  , responseState : Maybe Uuid
  , requestState : Maybe Uuid
  , auth : Maybe String
  , self : User
  , follows : List Follow
  , users : List User
  , videos : List Video
  , pendingUsers : List String
  , pendingVideos : List String
  , pendingRequests : List (Cmd Msg)
  , outstandingRequests : Int
  }

main = Navigation.program CurrentUrl
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = (\model -> Html.map UI (View.view model))
  }

init : Location -> (Model, Cmd Msg)
init location =
  let
    auth = extractHashArgument "access_token" location
    state = extractHashArgument "state" location
      |> Maybe.andThen Uuid.fromString
  in
  ( { location = location
    , responseState = state
    , requestState = Nothing
    , auth = auth
    , self = User "-" "-"
    , follows = []
    , users = []
    , videos = []
    , pendingUsers = []
    , pendingVideos = []
    , pendingRequests = []
    , outstandingRequests = 0
    }
  , Random.generate AuthState Uuid.uuidGenerator
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
    CurrentUrl location ->
      ( { model | location = location }, Cmd.none)
    Self (Ok (user::_)) ->
      ( { model
        | self = user
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
        , pendingVideos = List.append model.pendingVideos userIds
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
      | users = List.append model.users users
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
    UI (View.None) ->
      (model, Cmd.none)

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
      (60*Time.second/authRateLimit)
    Nothing ->
      (60*Time.second/rateLimit)

fetchSelfUrl : String
fetchSelfUrl =
  "https://api.twitch.tv/helix/users"

fetchSelf : Maybe String -> Cmd Msg
fetchSelf auth =
  helix <|
    { tagger = Self
    , auth = auth
    , url = fetchSelfUrl
    , decoder = Twitch.Deserialize.users
    }

fetchUsersUrl : List String -> String
fetchUsersUrl users =
  "https://api.twitch.tv/helix/users?id=" ++ (String.join "&id=" users)

fetchUsers : Maybe String -> List String -> Cmd Msg
fetchUsers auth users =
  if List.isEmpty users then
    Cmd.none
  else
    helix <|
      { tagger = Users
      , auth = auth
      , url = (fetchUsersUrl users)
      , decoder = Twitch.Deserialize.users
      }

fetchFollowsUrl : List String -> String
fetchFollowsUrl userIds =
  "https://api.twitch.tv/helix/users/follows?first=100&from_id=" ++ (String.join "&from_id=" userIds)

fetchFollows : Maybe String -> List String -> Cmd Msg
fetchFollows auth userIds =
  if List.isEmpty userIds then
    Cmd.none
  else
    helix <|
      { tagger = Follows
      , auth = auth
      , url = (fetchFollowsUrl userIds)
      , decoder = Twitch.Deserialize.follows
      }

fetchVideosUrl : String -> String
fetchVideosUrl userId =
  "https://api.twitch.tv/helix/videos?first=3&period=week&user_id=" ++ userId

fetchVideos : Maybe String -> String -> Cmd Msg
fetchVideos auth userId =
  helix <|
    { tagger = Videos
    , auth = auth
    , url = (fetchVideosUrl userId)
    , decoder = Twitch.Deserialize.videos
    }

helix :
  { tagger : ((Result Http.Error a) -> Msg)
  , auth : Maybe String
  , url : String
  , decoder : Json.Decode.Decoder a
  } -> Cmd Msg
helix {tagger, auth, url, decoder} =
  Http.send tagger <| Http.request
    { method = "GET"
    , headers =
      List.append
        [ Http.header "Client-ID" Twitch.Id.clientId
        ] (authHeaders auth)
    , url = url
    , body = Http.emptyBody
    , expect = Http.expectJson decoder
    , timeout = Nothing
    , withCredentials = False
    }

authHeaders : Maybe String -> List Http.Header
authHeaders auth =
  case auth of
    Just token -> 
      [ Http.header "Authorization" ("Bearer "++token) ]
    Nothing ->
      []

extractHashArgument : String -> Location -> Maybe String
extractHashArgument key location =
  location.hash
    |> String.dropLeft 1
    |> String.split "&"
    |> List.map (String.split "=")
    |> List.filter (\x -> case List.head x of
      Just s ->
        s == key
      Nothing ->
        False)
    |> List.head
    |> Maybe.andThen List.tail
    |> Maybe.andThen List.head
