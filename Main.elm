import StartApp
import Mouse
import Time exposing (fps)
import Task exposing (Task)
import Effects exposing (Never)
import View exposing (view)
import Model exposing (initial)
import Update exposing (update)
import Actions exposing (..)


addPointSignal : Signal Action
addPointSignal = Signal.map AddPoint (Signal.sampleOn Mouse.clicks Mouse.position)


app =
    StartApp.start
        { init = initial (1000, 1000)
        , update = update
        , view = view
        , inputs = 
            [ addPointSignal
            , Signal.map Tick (Time.fps 60)
            ]
        }


port tasks : Signal (Task.Task Never ())
port tasks =
    app.tasks


main =
    app.html


