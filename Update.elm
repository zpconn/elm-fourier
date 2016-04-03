module Update (update) where
import Model exposing (..)
import Actions exposing (..)
import Complex exposing (toComplex)
import Fourier exposing (computeFourierCoefficients, fourierPoint, recenterSamplePoints)
import Effects exposing (Effects)
import Time exposing (Time, second, inSeconds)
import Mouse


defaultSampleRange : Int
defaultSampleRange = 50 


defaultLoopDuration : Time
defaultLoopDuration = 5 * Time.second


update : Action -> Model -> (Model, Effects Action)
update msg model =
    case msg of
        Init time ->
            (model, Effects.none)

        Load encodedModel ->
            (model, Effects.none)

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

                loopedBackPoints = case (List.head model.points) of
                                       Just p -> p :: newPointList
                                       Nothing -> newPointList

                recenteredComplexPoints = 
                    recenterSamplePoints (List.map Complex.toComplex loopedBackPoints)
                                         (model.width, model.height)

                newFourierCoefficients =
                    computeFourierCoefficients recenteredComplexPoints
                                               (round ((toFloat (List.length recenteredComplexPoints)) / 5.0))
                                               
                newModel = { model |
                               points = newPointList,
                               fourierCoefficients = newFourierCoefficients }
            in
               (newModel, Effects.none)

        Pause ->
            (model, Effects.none)

        Resume ->
            (model, Effects.none)


