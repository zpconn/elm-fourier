module Update exposing (update)


import Model exposing (..)
import Actions exposing (..)
import Complex exposing (toComplex)
import Fourier exposing (computeFourierCoefficients, fourierPoint, recenterSamplePoints, FourierCoefficients)
import Task exposing (map, andThen, succeed)
import Time exposing (Time, second, inSeconds)


defaultSampleRange : Int
defaultSampleRange = 50


defaultLoopDuration : Float
defaultLoopDuration = 5000 


computeFourierCoefficientsFromSamples : (Int,Int) -> List (Int,Int) -> FourierCoefficients
computeFourierCoefficientsFromSamples (width, height) points =
    let
        loopedBackPoints = if (List.length points) <= 1 then
                              points
                           else
                              List.concat
                                  [points
                                  , (case (List.head points) of
                                       Just p -> [p]
                                       Nothing -> [])]


        recenteredComplexPoints =
            recenterSamplePoints (List.map Complex.toComplex loopedBackPoints)
                                 (width, height)
    in
        computeFourierCoefficients recenteredComplexPoints
                                   (round ((toFloat (List.length recenteredComplexPoints)) / 5.0))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Load encodedModel ->
            let
                pointList = case (decodePointList encodedModel) of
                                Err err -> []
                                Ok points -> points

                newFourierCoefficients =
                    computeFourierCoefficientsFromSamples (model.width, model.height)
                                                          pointList

                newModel = { model |
                               points = pointList,
                               fourierCoefficients = newFourierCoefficients }
            in
                (newModel, Cmd.none)

        Tick timeDelta ->
            let
                dt = timeDelta / defaultLoopDuration

                newNormalizedClock = advanceNormalizedClock model.normalizedClock dt

                currentPoint = case (clockToFloat newNormalizedClock) of
                                   Just t -> Just (fourierPoint model.fourierCoefficients t)
                                   Nothing -> Nothing

                newModel = { model |
                               normalizedClock = newNormalizedClock,
                               currentPoint = currentPoint }

            in
                (newModel, Cmd.none)

        AddPoint {x, y} ->
            let
                newPointList = (x, y) :: model.points

                newFourierCoefficients =
                    computeFourierCoefficientsFromSamples (model.width, model.height)
                                                          newPointList

                newModel = { model |
                               points = newPointList,
                               fourierCoefficients = newFourierCoefficients }

                encodedPointList = encodePointList newPointList
            in
               (newModel, Cmd.none)

        Pause ->
            (model, Cmd.none)

        Resume ->
            (model, Cmd.none)


