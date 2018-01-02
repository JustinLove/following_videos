module View exposing (Msg(..), view)

import Twitch.Deserialize exposing (User)
import Twitch.Template exposing (imagePercentTemplateUrl)
import Twitch.Id

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Keyed as Keyed
import Svg exposing (svg, use)
import Svg.Attributes exposing (xlinkHref)
import Http
import Navigation exposing (Location)
import Uuid exposing (Uuid)

type Msg
  = Refresh

css = """
body {
  background-color: rgb(23, 20, 31);
  color: rgb(218, 216, 222);
}
header { padding-left: 40px; }
svg.icon {
  display: inline-block;
  width: 1em;
  height: 1em;
  vertical-align: -0.2em;
  stroke-width: 0;
  stroke: currentColor;
  fill: currentColor;
}
.icon-github { color: #888; }
.icon-twitter { color: #55acee; }
.icon-twitch { color: #6441A4; }
#videos { display: flex; flex-wrap: wrap; list-style-type: none;}
.video { width: 240px; height: 200px; padding: 10px; }
.info { display: flex; overflow-x: hidden; }
.game-image { flex-shrink: 0; margin-right: 0.5em; }
.info-text p { margin: 0.2em; font-size: 0.8em; white-space: nowrap; }
a:link, a:visited { color: #b19dd8; }
a:hover, a:active { color: rgb(218, 216, 222); }
"""

--view : Model -> Html Msg
view model =
  div []
    [ node "style" [] [ text css ]
    , displayHeader model
    , Keyed.ul [ id "videos" ]
      <| List.map (videoView model)
      <| List.reverse
      <| List.sortBy .publishedAt model.videos
    , footer []
      [ a [ href "https://github.com/JustinLove/following_videos" ]
        [ icon "github", text "following_videos" ]
      , text " "
      , a [ href "https://twitter.com/wondible" ]
        [ icon "twitter", text "@wondible" ]
      , text " "
      , a [ href "https://twitch.tv/wondible" ]
        [ icon "twitch", text "wondible" ]
      ]
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
  ( video.id
  , li [ class "video" ]
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
  )

displayNameFor : List User -> String -> String
displayNameFor users userId =
  List.filterMap (\u -> if u.id == userId then Just u.displayName else Nothing) users
   |> List.head
   |> Maybe.withDefault "unknown"

displayLogin model =
  case model.auth of
    Just _ ->
      span []
        [ span [ class "user", title model.self.id ] [ text model.self.displayName ]
        , text " "
        , a [ href (model.location.origin ++ model.location.pathname) ] [ text "logout" ]
        , text " "
        , button [onClick Refresh] [ text "Refresh" ]
        ]
    Nothing ->
      a [ href (authorizeUrl (urlForRedirect model.location) model.requestState) ] [ text "login" ]

authorizeUrl : String -> Maybe Uuid -> String
authorizeUrl redirectUri authState =
  "https://api.twitch.tv/kraken/oauth2/authorize"
    ++ "?client_id=" ++ Twitch.Id.clientId
    ++ "&redirect_uri=" ++ (Http.encodeUri redirectUri)
    ++ "&response_type=token"
    ++ (case authState of
      Just uuid -> "&state=" ++ (Uuid.toString uuid)
      Nothing -> "")

urlForRedirect : Location -> String
urlForRedirect location =
  location.href
    |> String.dropRight (String.length location.hash)
    |> String.dropRight (String.length location.search)

icon : String -> Html Msg
icon name =
  svg [ Svg.Attributes.class ("icon icon-"++name) ]
    [ use [ xlinkHref ("#icon-"++name) ] [] ]
