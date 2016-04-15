module Update (update) where
import Model exposing (..)
import Actions exposing (..)
import Complex exposing (toComplex)
import Fourier exposing (computeFourierCoefficients, fourierPoint, recenterSamplePoints, FourierCoefficients)
import Effects exposing (Effects)
import Task exposing (map, toMaybe, andThen, succeed)
import Time exposing (Time, second, inSeconds)
import History exposing (setHash)
import Mouse


defaultSampleRange : Int
defaultSampleRange = 50 


defaultLoopDuration : Time
defaultLoopDuration = 15 * Time.second


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


update : Action -> Model -> (Model, Effects Action)
update msg model =
    case msg of
        Init time ->
            (model, Effects.none)

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
                (newModel, Effects.none)

        Tick timeDelta ->
            let
                dt = (Time.inSeconds timeDelta) / (Time.inSeconds defaultLoopDuration)

                newNormalizedClock = advanceNormalizedClock model.normalizedClock dt

                currentPoint = case (clockToFloat newNormalizedClock) of
                                   Just t -> Just (fourierPoint model.fourierCoefficients t)
                                   Nothing -> Nothing

                newModel = { model |
                               normalizedClock = newNormalizedClock,
                               currentPoint = currentPoint }

            in
                (newModel, Effects.none)
            
        AddPoint (x, y) ->
            let
                newPointList = (x, y) :: model.points

                newFourierCoefficients =
                    computeFourierCoefficientsFromSamples (model.width, model.height)
                                                          newPointList
                                               
                newModel = { model |
                               points = newPointList,
                               fourierCoefficients = newFourierCoefficients }

                encodedPointList = encodePointList newPointList

                effects = (History.setHash encodedPointList) `Task.andThen` (always (Task.succeed encodedPointList))
                        |> Task.toMaybe
                        |> Task.map HashUpdated
                        |> Effects.task
            in
               (newModel, effects)

        Pause ->
            (model, Effects.none)

        Resume ->
            (model, Effects.none)

        HashUpdated x ->
            (model, Effects.none)


