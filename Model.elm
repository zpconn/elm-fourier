module Model exposing 
    (Model, 
    State(..), 
    AnimationState, 
    initial, 
    NormalizedClock(..), 
    clockToFloat, 
    advanceNormalizedClock, 
    encodePointList, 
    decodePointList)


import Time exposing (Time)
import String exposing (concat)
import Json.Encode exposing (encode, object, int, list, Value)
import Json.Decode exposing (Decoder, int, map2, decodeString, field)
import Actions exposing (Msg)
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


initial : (Int, Int) -> (Model, Cmd Msg)
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
    , Cmd.none )


pointToJSONObj : (Int,Int) -> Json.Encode.Value
pointToJSONObj (x,y) =
    Json.Encode.object [ ("x", Json.Encode.int x), ("y", Json.Encode.int y) ]


encodePointList : List (Int,Int) -> String
encodePointList points =
    let
        pointObjects = Json.Encode.list (List.map pointToJSONObj points)
    in
        Json.Encode.encode 0 pointObjects


pointDecoder : Json.Decode.Decoder (Int,Int)
pointDecoder =
    Json.Decode.map2 (,)
        (field "x" Json.Decode.int)
        (field "y" Json.Decode.int)


pointListDecoder : Json.Decode.Decoder (List (Int,Int))
pointListDecoder =
    Json.Decode.list pointDecoder


decodePointList : String -> Result String (List (Int,Int))
decodePointList s =
    Json.Decode.decodeString pointListDecoder s


