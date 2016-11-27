module Actions exposing (Msg(..))
import Mouse exposing (Position)


type Msg
    = Load String
    | Tick Float
    | AddPoint Mouse.Position
    | Pause
    | Resume


