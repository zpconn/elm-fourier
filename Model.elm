module Model (Model, State(..), AnimationState, initial) where
import Time exposing (Time)
import Effects exposing (Effects)
import Actions exposing (Action)
import Complex
import Fourier exposing (FourierCoefficients)


type State = Paused | Playing


type alias AnimationState =
    Maybe { prevClockTime : Time, elapsedTime : Time }


type alias Model =
    { width : Int
    , height : Int
    , points : List (Int,Int)
    , state : State
    , animationState : AnimationState
    , fourierCoefficients : FourierCoefficients
    }


initial : (Int, Int) -> (Model, Effects Action)
initial (w,h) = 
    ( { width = w
      , height = h
      , points = []
      , state = Playing
      , animationState = Nothing
      , fourierCoefficients = { sampleRange = 0, coefficients = [] }
      }
    , Effects.none )


