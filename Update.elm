module Update (update) where
import Model exposing (..)
import Actions exposing (..)
import Complex exposing (toComplex)
import Fourier exposing (computeFourierCoefficients)
import Effects exposing (Effects)
import Time exposing (Time)
import Mouse


defaultSampleRange : Int
defaultSampleRange = 10


update : Action -> Model -> (Model, Effects Action)
update msg model =
    case msg of
        Init time ->
            (model, Effects.none)

        Load encodedModel ->
            (model, Effects.none)

        Tick time ->
            (model, Effects.none)
            
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

