module Fourier (computeFourierCoefficients, fourierPoint, FourierCoefficients) where
import Complex exposing (Complex(..))


type alias Samples = List (Complex)


type alias FourierCoefficients =
    { sampleRange : Int
    , coefficients : List (Complex)
    }


fourierCoefficient : Int -> Samples -> Complex
fourierCoefficient n zs =
    let
        numSamples = List.length zs
        
        expTerm : Int -> Complex
        expTerm k =
            Complex.exp ( -2.0 * pi * (toFloat n) * (toFloat k) / (toFloat numSamples) )

        products = flip List.indexedMap zs (\k z -> Complex.mul z (expTerm k))

        sum = List.foldr Complex.add (Complex.purelyReal 0) products
    in
        Complex.mul ( Complex.purelyReal (1 / (toFloat numSamples)) ) sum


computeFourierCoefficients : Samples -> Int -> FourierCoefficients
computeFourierCoefficients zs sampleRange =
    let
        indices = [-sampleRange..sampleRange]
    in
        { sampleRange = sampleRange
        , coefficients = List.map (\n -> fourierCoefficient n zs) indices
        }


fourierPoint : FourierCoefficients -> Float -> Complex
fourierPoint {sampleRange, coefficients} t =
    let
        expTerm : Int -> Complex
        expTerm n =
            Complex.exp ( 2 * pi * (toFloat n) * t )

        indices = [-sampleRange..sampleRange]

        products = flip List.map (List.map2 (,) indices coefficients) (\(n, c) -> Complex.mul c (expTerm n))
    in
        List.foldr Complex.add (Complex.purelyReal 0) products


