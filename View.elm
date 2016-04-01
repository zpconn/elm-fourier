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
import Complex exposing (Complex(..))


pointRadius : Float
pointRadius = 3


drawPoint : (Int,Int) -> ((Int,Int),Color) -> Form
drawPoint (w,h) ((x,y),color) = 
    circle pointRadius
      |> filled color
      |> move (toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y)


drawUserPathVertices : (Int,Int) -> List (Int,Int) -> Maybe Complex -> Element
drawUserPathVertices (w,h) locs currentPoint =
    let
        coloredLocs = List.map (\p -> (p, hsla 0.3 0.9 0.6 0.7)) locs

        newColoredLocs = case currentPoint of
                             Nothing -> coloredLocs
                             Just (Complex x y) -> ((round x, round y), hsla 230 1 0.5 1) :: coloredLocs
    in
        layers
          [ collage w h (List.map (drawPoint (w,h)) newColoredLocs) ]


view : Signal.Address Action -> Model -> Html
view address model =
    div 
        []
        [fromElement (drawUserPathVertices (model.width, model.height) model.points model.currentPoint)] 


