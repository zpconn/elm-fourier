module Model (Model, State(..), AnimationState, initial) where
import Time exposing (Time)
import Effects exposing (Effects)
import Actions exposing (Action)
import Complex exposing (Complex(..))
import Fourier exposing (FourierCoefficients)


type State = Paused | Playing


type alias AnimationState =
    Maybe { prevClockTime : Time, elapsedTime : Time }


type alias Model =
    { width : Int
    , height : Int
    , points : List (Int,Int)
    , state : State
    , normalizedClock : Float
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
      , normalizedClock = 0.0
      , animationState = Nothing
      , fourierCoefficients = { sampleRange = 0, coefficients = [] }
      , currentPoint = Nothing
      }
    , Effects.none )


