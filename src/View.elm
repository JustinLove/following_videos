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
.channel {
  background-color: rgb(14, 12, 19);
  color: rgb(250, 249, 250);
  border: 1px solid #392e5c;
}
"""

--view : Model -> Html Msg
view model =
  div []
    [ node "style" [] [ text css ]
    , ul [ id "videos" ] <| List.map (.to_id >> text >> List.singleton >> li [ class "video"]) model.follows
    ]
