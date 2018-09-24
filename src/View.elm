module View exposing (Msg(..), view, document)

import Persist exposing (User)
import Twitch.Template exposing (imagePercentTemplateUrl)
import TwitchId

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Keyed as Keyed
import Svg exposing (svg, use)
import Svg.Attributes exposing (xlinkHref)
import Http
import Uuid exposing (Uuid)
import Time exposing (Posix, Zone, Month(..))
import Url exposing (Url)
import Url.Builder as Url

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

document tagger model =
  { title = "Following Videos"
  , body = [Html.map tagger (view model)]
  }

--view : Model -> Html Msg
view model =
  div []
    [ node "style" [] [ text css ]
    , displayHeader model
    , Keyed.ul [ id "videos" ]
      <| List.map (videoView model)
      <| List.reverse
      <| List.sortBy (.publishedAt>>Time.posixToMillis) model.videos
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
    , span [ class "follows" ] [ text <| String.fromInt <| List.length model.follows ]
    , text " "
    , progress
      [ Html.Attributes.max <| String.fromInt <| List.length model.follows
      , value <| String.fromInt <| List.length model.pendingRequests
      ] []
    , text " "
    , span [ class "pending" ] [ text <| String.fromInt <| List.length model.pendingRequests ]
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
        , p [ class "date" ] [ text <| dateString model.zone video.publishedAt ]
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
        , a [ href (Url.relative [] []) ] [ text "logout" ]
        , text " "
        , button [onClick Refresh] [ text "Refresh" ]
        ]
    Nothing ->
      a [ href (authorizeUrl (urlForRedirect model.location) model.requestState) ] [ text "login" ]

authorizeUrl : String -> Maybe Uuid -> String
authorizeUrl redirectUri authState =
  "https://api.twitch.tv/kraken/oauth2/authorize"
    ++ (
      List.append
        [ Url.string "client_id" TwitchId.clientId
        , Url.string "redirect_uri" redirectUri
        , Url.string "response_type" "token"
        ]
        (case authState of
          Just uuid -> [ Url.string "state" (Uuid.toString uuid) ]
          Nothing -> [])
      |> Url.toQuery
      )

urlForRedirect : Url -> String
urlForRedirect url =
  {url | query = Nothing, fragment = Nothing } |> Url.toString

icon : String -> Html Msg
icon name =
  svg [ Svg.Attributes.class ("icon icon-"++name) ]
    [ use [ xlinkHref ("symbol-defs.svg#icon-"++name) ] [] ]

dateString : Zone -> Posix -> String
dateString zone time =
  (String.fromInt <| Time.toYear zone time) ++ "-" ++
  (monthNum <| Time.toMonth zone time) ++ "-" ++
  (numberPad <| Time.toDay zone time)

monthNum : Month -> String
monthNum mon =
  case mon of
    Jan -> "01"
    Feb -> "02"
    Mar -> "03"
    Apr -> "04"
    May -> "05"
    Jun -> "06"
    Jul -> "07"
    Aug -> "08"
    Sep -> "09"
    Oct -> "10"
    Nov -> "11"
    Dec -> "12"

numberPad : Int -> String
numberPad i =
  if i < 10 then
    "0" ++ (String.fromInt i)
  else
    String.fromInt i
