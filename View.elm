module View exposing (view)
import Color exposing (..)
import Element exposing (..)
import Collage exposing (..)
import Html exposing (div, Html, text, button)
import Html.Events exposing (onClick)
import Model exposing (Model, NormalizedClock(..), clockToFloat)
import Actions exposing (Msg)
import Complex exposing (Complex(..))
import Fourier exposing (Circle(..), computeCirclesFromCoefficients)


pointRadius : Float
pointRadius = 3


drawCircles : (Int,Int) -> List (Circle) -> List (Form)
drawCircles (w,h) circles =
    let
        constructCircle : Circle -> Form
        constructCircle (Circle (Complex x y) r) =
            circle r |> outlined defaultLine
                     |> move (x, -y)
    in
        List.map constructCircle circles


drawPoint : (Int,Int) -> ((Int,Int),Color) -> Form
drawPoint (w,h) ((x,y),color) =
    circle pointRadius
      |> filled color
      |> move (toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y)


drawSamplePath : (Int,Int) -> List (Int,Int) -> Form
drawSamplePath (w,h) locs =
    let
        points = flip List.map locs (\(x,y) -> (toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y))

        samplePath = path points
    in
        traced defaultLine samplePath


drawEverything : (Int,Int) -> List (Int,Int) -> Maybe Complex -> Model -> Element
drawEverything (w,h) locs currentPoint model =
    let
        coloredLocs = List.map (\p -> (p, hsla 0.3 0.9 0.6 0.7)) locs

        newColoredLocs = case currentPoint of
                             Nothing -> coloredLocs
                             Just (Complex x y) ->
                                 let
                                     recentered = ((round x) + round(toFloat w / 2),
                                                   (round y) + round(toFloat h / 2))
                                 in
                                     (recentered, hsla 230 1 0.5 1) :: coloredLocs

        circles = case (clockToFloat model.normalizedClock) of
                      Just t -> computeCirclesFromCoefficients model.fourierCoefficients t
                      Nothing -> []

        elements = (List.map (drawPoint (w,h)) newColoredLocs) ++
                   (drawCircles (w,h) circles) ++
                   [drawSamplePath (w,h) locs]
    in
        layers
          [ collage w h elements ]


view : Model -> Html Msg
view model =
    div
        []
        [ toHtml <| (drawEverything (model.width, model.height) model.points model.currentPoint model) ]


