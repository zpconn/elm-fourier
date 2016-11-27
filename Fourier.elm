module Fourier exposing (computeFourierCoefficients, fourierPoint, FourierCoefficients, Circle(..), computeCirclesFromCoefficients, recenterSamplePoints)
import Complex exposing (Complex(..))


type alias Samples = List (Complex)


type alias FourierCoefficients =
    { sampleRange : Int
    , coefficients : List (Complex)
    }


type Circle = Circle Complex Float


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


recenterSamplePoints : Samples -> (Int, Int) -> Samples
recenterSamplePoints zs (w,h) =
    let
        center = Complex ((toFloat w) / 2) ((toFloat h) / 2)

        recenterPoint : Complex -> Complex
        recenterPoint p = Complex.sub p center
    in
        List.map recenterPoint zs


computeFourierCoefficients : Samples -> Int -> FourierCoefficients
computeFourierCoefficients zs sampleRange =
    let
        indices = List.range -sampleRange sampleRange
    in
        { sampleRange = sampleRange
        , coefficients = List.map (\n -> fourierCoefficient n zs) indices
        }


partialSum : FourierCoefficients -> Float -> Int -> Complex
partialSum {sampleRange, coefficients} t stopIdx =
    let
        expTerm : Int -> Complex
        expTerm n =
            Complex.exp ( 2 * pi * (toFloat n) * t )

        indices = List.range -sampleRange stopIdx

        products = flip List.map (List.map2 (,) indices coefficients) (\(n, c) -> Complex.mul c (expTerm n))
    in
        List.foldr Complex.add (Complex.purelyReal 0) products


fourierPoint : FourierCoefficients -> Float -> Complex
fourierPoint coeffs t = partialSum coeffs t coeffs.sampleRange


approximateFourierPath : FourierCoefficients -> Int -> List (Complex)
approximateFourierPath coeffs numPoints =
    let
        ts = List.map (\i -> (toFloat i) / (toFloat (numPoints - 1))) (List.range 0 (numPoints-1))
    in
        List.map (\t -> fourierPoint coeffs t) ts


computeCirclesFromCoefficients : FourierCoefficients -> Float -> List (Circle)
computeCirclesFromCoefficients coeffs t =
    let
        indices = List.range -coeffs.sampleRange coeffs.sampleRange

        partialSums = List.map (partialSum coeffs t) indices

        shiftedPartialSums = (Complex 0 0) :: partialSums
    in
        List.map2 (\c s -> Circle s (Complex.mag c)) coeffs.coefficients shiftedPartialSums


