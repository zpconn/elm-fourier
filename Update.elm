module Update (update) where
import Model exposing (..)
import Actions exposing (..)
import Complex exposing (toComplex)
import Fourier exposing (computeFourierCoefficients, fourierPoint)
import Effects exposing (Effects)
import Time exposing (Time, second, inSeconds)
import Mouse


defaultSampleRange : Int
defaultSampleRange = 10


defaultLoopDuration : Time
defaultLoopDuration = 20 * Time.second


update : Action -> Model -> (Model, Effects Action)
update msg model =
    case msg of
        Init time ->
            (model, Effects.none)

        Load encodedModel ->
            (model, Effects.none)

        Tick timeDelta ->
            let
                incrementedClock = model.normalizedClock + ( (Time.inSeconds timeDelta) / (Time.inSeconds defaultLoopDuration) )

                newNormalizedClock = if incrementedClock <= 1.0 then
                                          incrementedClock
                                     else
                                          0.0 + (incrementedClock - 1.0)

                currentPoint = fourierPoint model.fourierCoefficients newNormalizedClock

                newModel = { model | normalizedClock = newNormalizedClock }
            in
                (newModel, Effects.none)
            
        AddPoint (x, y) ->
            let
                newPointList = (x, y) :: model.points

                newFourierCoefficients =
                    computeFourierCoefficients (List.map Complex.toComplex newPointList)
                                               defaultSampleRange
                                               
                newModel = { model |
                               points = newPointList,
                               fourierCoefficients = newFourierCoefficients }
            in
               (newModel, Effects.none)

        Pause ->
            (model, Effects.none)

        Resume ->
            (model, Effects.none)


