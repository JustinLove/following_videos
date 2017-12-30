import Twitch.Deserialize exposing (User, Follow, Video)
import Twitch.Id
import View

import Html
import Navigation exposing (Location)
import Http
import Time
import Json.Decode

requestLimit = 100
rateLimit = 30
authRateLimit = 120
videoLimit = requestLimit

type Msg
  = Self (Result Http.Error (List User))
  | Follows (Result Http.Error (List Follow))
  | Users (Result Http.Error (List User))
  | Videos (Result Http.Error (List Video))
  | NextRequest Time.Time
  | CurrentUrl Location
  | UI (View.Msg)

type alias Model =
  { location : Location
  , auth : Maybe String
  , token : Maybe String
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
    token = extractHashArgument "id_token" location
  in
  ( { location = location
    , auth = auth
    , token = token
    , self = User "-" "-"
    , follows = []
    , users = []
    , videos = []
    , pendingUsers = []
    , pendingVideos = []
    , pendingRequests = case auth of
      Just _ ->
        [ fetchSelf auth ]
      Nothing ->
        []
    , outstandingRequests = 0
    }
  , Cmd.none
  )

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
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
        , pendingVideos = List.append model.pendingVideos userIds
        , pendingRequests = List.append model.pendingRequests
          ((fetchUsers model.auth userIds) :: (List.take videoLimit <| List.map (fetchVideos model.auth) userIds))
        }
      , Cmd.none
      )
    Follows (Err error) ->
      let _ = Debug.log "follow fetch error" error in
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
    UI (View.None) ->
      (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  if List.isEmpty model.pendingRequests then
    Sub.none
  else
    Time.every ((requestRate model.auth)*1.05) NextRequest

requestRate : Maybe String -> Float
requestRate auth =
  case auth of
    Just _ ->
      (60*Time.second/authRateLimit)
    Nothing ->
      (60*Time.second/rateLimit)

fetchNextUserBatch : Int -> Model -> Model
fetchNextUserBatch batch model =
  { model
  | pendingUsers = List.drop batch model.pendingUsers
  , pendingRequests = List.append model.pendingRequests
    [fetchUsers model.auth <| List.take batch model.pendingUsers]
  }

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
