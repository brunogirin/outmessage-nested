port module Main exposing (..)

import OutMessage.NestedTest
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)


main : TestProgram
main =
    run emit OutMessage.NestedTest.all


port emit : ( String, Value ) -> Cmd msg
