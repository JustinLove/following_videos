module Twitch.Template exposing (imageTemplateUrl)

import Regex

imageTemplateUrl : Int -> Int -> String -> String
imageTemplateUrl w h =
  Regex.replace Regex.All (Regex.regex "\\{width\\}") (\_ -> toString w)
  >> Regex.replace Regex.All (Regex.regex "\\{height\\}") (\_ -> toString h)
