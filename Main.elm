import StartApp
import Mouse
import View exposing (view)
import Model exposing (initial)
import Update exposing (update)
import Actions exposing (..)


addPointSignal : Signal Action
addPointSignal = Signal.map AddPoint (Signal.sampleOn Mouse.clicks Mouse.position)


app =
    StartApp.start
        { init = initial (500, 500)
        , update = update
        , view = view
        , inputs = 
            [ addPointSignal ]
        }


main =
    app.html


