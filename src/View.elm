module View exposing (Msg(..), view)

import Twitch.Deserialize exposing (User)
import Twitch.Template exposing (imagePercentTemplateUrl)
import Twitch.Id

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events

type Msg
  = None

css = """
body {
  background-color: rgb(23, 20, 31);
  color: rgb(218, 216, 222);
}
header { padding-left: 40px; }
#videos { display: flex; flex-wrap: wrap; list-style-type: none;}
.video { width: 240px; height: 200px; padding: 10px; }
.info { display: flex; overflow-x: hidden; }
.game-image { flex-shrink: 0; margin-right: 0.5em; }
.info-text p { margin: 0.2em; font-size: 0.8em; white-space: nowrap; }
"""

--view : Model -> Html Msg
view model =
  div []
    [ node "style" [] [ text css ]
    , displayHeader model
    , ul [ id "videos" ]
      <| List.map (videoView model)
      <| List.reverse
      <| List.sortBy .publishedAt model.videos
    ]

displayHeader model =
  header []
    [ displayLogin model
    , text " following "
    , span [ class "follows" ] [ text <| toString <| List.length model.follows ]
    , text " "
    , progress
      [ Html.Attributes.max <| toString <| List.length model.follows
      , value <| toString <| List.length model.pendingRequests
      ] []
    , text " "
    , span [ class "pending" ] [ text <| toString <| List.length model.pendingRequests ]
    , text " "
    ]

videoView model video =
  let
    name = displayNameFor model.users video.userId
  in
  li [ class "video" ]
    [ a [ href ("https://twitch.tv/videos/"++video.id) ] [ img [ class "preview", src (imagePercentTemplateUrl 320 180 video.thumbnailUrl), width 239, height 134 ] [] ]
    , div [ class "info" ]
      [ div [ class "info-text" ]
        [ p [ class "title", title video.title ] [ text video.title ]
        , br [] []
        , p [ class "name" ] [ text name ]
        , p [ class "date" ] [ text video.publishedAt ]
        ]
      ]
    ]

displayNameFor : List User -> String -> String
displayNameFor users userId =
  List.filterMap (\u -> if u.id == userId then Just u.displayName else Nothing) users
   |> List.head
   |> Maybe.withDefault "unknown"

displayLogin model =
  case model.auth of
    Just _ ->
      span [ class "user", title model.self.id ] [ text model.self.displayName ]
    Nothing ->
      a [ href (authorizeUrl "http://localhost:8000/src/FollowingVideos.elm") ] [ text "login" ]

authorizeUrl : String -> String
authorizeUrl redirectUri =
  "https://api.twitch.tv/kraken/oauth2/authorize"
    ++ "?client_id=" ++ Twitch.Id.clientId
    ++ "&redirect_uri=" ++ redirectUri
    ++ "&response_type=token+id_token"
    ++ "&scope=openid"
    -- ++ "&state=" ++ some random value
