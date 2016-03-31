module View (view) where
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Mouse
import Window
import Html exposing (div, Html, text, button, fromElement)
import Html.Events exposing (onClick)
import Model exposing (Model)
import Actions exposing (Action)


pointRadius : Float
pointRadius = 3


drawPoint : (Int,Int) -> (Int,Int) -> Form
drawPoint (w,h) (x,y) = 
    circle pointRadius
      |> filled (hsla 0.3 0.9 0.6 0.7)
      |> move (toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y)


drawUserPathVertices : (Int,Int) -> List (Int,Int) -> Element
drawUserPathVertices (w,h) locs =
    layers
      [ collage w h (List.map (drawPoint (w,h)) locs)
      , show "Click to place a new point."
      ]


view : Signal.Address Action -> Model -> Html
view address model =
    div 
        []
        [fromElement (drawUserPathVertices (model.width, model.height) model.points)]


