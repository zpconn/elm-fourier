import Html as Html
import AnimationFrame exposing (diffs)
import Mouse exposing (..)
import Task exposing (Task)
import View exposing (view)
import Model exposing (initial)
import Update exposing (update)
import Actions exposing (..)


main =
    Html.program
        { init = initial (1000, 1000)
        , update = update
        , view = view
        , subscriptions =
            \_ -> Sub.batch
                [ Mouse.clicks AddPoint
                , AnimationFrame.diffs Tick
                ]
        }


