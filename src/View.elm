module View exposing (Msg(..), view)

import Twitch.Deserialize exposing (User)
import Twitch.Template exposing (imageTemplateUrl)

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
    , ul [ id "videos" ] <| List.map (videoView model) model.users
    ]

videoView model user =
  let
    name = user.displayName
  in
  li [ class "video" ]
    [ a [ href ("https://twitch.tv/"++name) ] [ text name ]
    , div [ class "info" ]
      [ div [ class "info-text" ]
        [ p [ class "title", title name ] [ text name ]
        , br [] []
        , p [ class "name" ] [ text name ]
        ]
      ]
    ]
