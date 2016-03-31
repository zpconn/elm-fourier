module Actions (Action(..)) where
import Time exposing (Time)


type Action
    = Init Time
    | Load String
    | Tick Time
    | AddPoint (Int, Int)
    | Pause
    | Resume

