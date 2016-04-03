module Model (Model, State(..), AnimationState, initial, NormalizedClock(..), clockToFloat, advanceNormalizedClock) where
import Time exposing (Time)
import Effects exposing (Effects)
import Actions exposing (Action)
import Complex exposing (Complex(..))
import Fourier exposing (FourierCoefficients)


type State = Paused | Playing


type alias AnimationState =
    Maybe { prevClockTime : Time, elapsedTime : Time }


type NormalizedClock = NormalizedClock Float


clockToFloat : NormalizedClock -> Maybe Float
clockToFloat (NormalizedClock t) =
    if t >= 0 && t <= 1 then
       Just t
    else
       Nothing


advanceNormalizedClock : NormalizedClock -> Float -> NormalizedClock
advanceNormalizedClock (NormalizedClock t) dt =
    let
        normalizedDt = dt - toFloat (floor dt)

        incrementedClock = t + normalizedDt
    in
        if incrementedClock > 1.0 then
            NormalizedClock (incrementedClock - 1.0)
        else
            NormalizedClock incrementedClock


type alias Model =
    { width : Int
    , height : Int
    , points : List (Int,Int)
    , state : State
    , normalizedClock : NormalizedClock
    , animationState : AnimationState
    , fourierCoefficients : FourierCoefficients
    , currentPoint : Maybe Complex
    }


initial : (Int, Int) -> (Model, Effects Action)
initial (w,h) = 
    ( { width = w
      , height = h
      , points = []
      , state = Playing
      , normalizedClock = NormalizedClock 0.0
      , animationState = Nothing
      , fourierCoefficients = { sampleRange = 0, coefficients = [] }
      , currentPoint = Nothing
      }
    , Effects.none )


