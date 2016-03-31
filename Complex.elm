module Complex (Complex(..), add, mul, exp, purelyReal, purelyImag, toComplex) where


type Complex = Complex Float Float


add : Complex -> Complex -> Complex
add (Complex a b) (Complex c d) =
    Complex (a + c) (b + d)


mul : Complex -> Complex -> Complex
mul (Complex a b) (Complex c d) =
    Complex (a*c - b*d) (b*c + a*d)


exp : Float -> Complex
exp theta =
    Complex (cos theta) (sin theta)


purelyReal : Float -> Complex
purelyReal x =
    Complex x 0.0


purelyImag : Float -> Complex
purelyImag x =
    Complex 0 x


toComplex : (Int, Int) -> Complex
toComplex (x, y) = Complex (toFloat x) (toFloat y)


