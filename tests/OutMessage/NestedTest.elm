module OutMessage.NestedTest exposing (..)

-- where

import Test exposing (..)
import Expect
import OutMessage.Nested exposing (..)
import Result


type ChildOutMsg
    = Ping
    | PingPing


type ParentOutMsg
    = Pong
    | PongPong


type Msg
    = NoOp


interpret : ChildOutMsg -> String -> ( String, Cmd Msg, ParentOutMsg )
interpret child model =
    case child of
        Ping ->
            ( model, Cmd.none, Pong )

        PingPing ->
            ( model, Cmd.none, PongPong )


all : Test
all =
    describe "OutMessage.Nested package test suite"
        [ test "evaluate" <|
            \() ->
                Expect.equal
                    ( "test", Cmd.batch [ Cmd.none, Cmd.none ], Pong )
                    (evaluate interpret ( "test", Cmd.none, Ping ))
        , describe "evaluateMaybe"
            [ test "Nothing" <|
                \() ->
                    Expect.equal
                        ( "test", Cmd.none, Nothing )
                        (evaluateMaybe interpret Cmd.none ( "test", Cmd.none, Nothing ))
            , test "Just Ping" <|
                \() ->
                    Expect.equal
                        ( "test", Cmd.batch [ Cmd.none, Cmd.none ], Just Pong )
                        (evaluateMaybe interpret Cmd.none ( "test", Cmd.none, Just Ping ))
            ]
        , test "evaluateList" <|
            \() ->
                Expect.equal
                    ( "test", Cmd.batch [ Cmd.none, Cmd.batch [ Cmd.batch [ Cmd.none, Cmd.none ], Cmd.none ] ], [ Pong, PongPong ] )
                    (evaluateList interpret ( "test", Cmd.none, [ Ping, PingPing ] ))
        , describe "evaluateResult"
            [ test "Error" <|
                \() ->
                    Expect.equal
                        ( "test", Cmd.none, Result.Err "error" )
                        (evaluateResult interpret (\_ -> Cmd.none) ( "test", Cmd.none, Result.Err "error" ))
            , test "Just Ping" <|
                \() ->
                    Expect.equal
                        ( "test", Cmd.batch [ Cmd.none, Cmd.none ], Result.Ok Pong )
                        (evaluateResult interpret (\_ -> Cmd.none) ( "test", Cmd.none, Result.Ok Ping ))
            ]
        ]
