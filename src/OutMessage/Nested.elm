module OutMessage.Nested
    exposing
        ( evaluate
        , evaluateMaybe
        , evaluateList
        , evaluateResult
        , mapComponent
        , mapCmd
        , mapOutMsg
        )

{-|

The nested OutMsg pattern has three components:

* ChildOutMsg, a user-defined type (just like Model or Msg) with the specific
  purpose of receiving notifications from a child component.
* ParentOutMsg, a user-defined type with the specific purpose of notifying a
  parent component.
* `interpretOutMsg`, a function that converts ChildOutMsg values into side-effects
  (commands and changes to the model) and a ParentOutMsg.

ChildOutMsg values can be captured in the parent's update function, and handled
there by `interpretOutMsg`. The basic pattern can be extended to handle multiple
ChildOutMsg using List or to optionally handle no ChildOutMsg using Maybe.

#Evaluators
@docs evaluate, evaluateMaybe, evaluateList, evaluateResult

#Mapping
@docs mapComponent, mapCmd, mapOutMsg
-}

import OutMessage
import State exposing (State)
import Result exposing (Result)


{-| Turn a child `OutMsg` value into commands, model changes and parent `OutMsg` value.
-}
evaluate :
    (childOutMsg -> model -> ( model, Cmd msg, parentOutMsg ))
    -> ( model, Cmd msg, childOutMsg )
    -> ( model, Cmd msg, parentOutMsg )
evaluate interpretOutMsg ( model, cmd, childOutMsg ) =
    wrap interpretOutMsg childOutMsg
        |> run cmd model


{-| Turn a child `Maybe OutMsg` into effects, model changes and parent `Maybe OutMsg` value.

Has a third argument for a default command that is used when child OutMsg is Nothing.
This method assumes that if the child message is `Nothing` then so is the parent
message and that if the child message is not `Nothing`, then the parent isn't either.
If this is not the case, use the `evaluate` method with an `interpret` method that
handle `Maybe` cases.
-}
evaluateMaybe :
    (childOutMsg -> model -> ( model, Cmd msg, parentOutMsg ))
    -> Cmd msg
    -> ( model, Cmd msg, Maybe childOutMsg )
    -> ( model, Cmd msg, Maybe parentOutMsg )
evaluateMaybe interpretOutMsg default ( model, cmd, mChildOutMsg ) =
    case mChildOutMsg of
        Nothing ->
            ( model, default, Nothing )

        Just childOutMsg ->
            let
                ( newModel, newCmd, parentOutMsg ) =
                    evaluate interpretOutMsg ( model, cmd, childOutMsg )
            in
                ( newModel, newCmd, Just parentOutMsg )


{-| Turn a `List ChildOutMsg` into effects, model changes and `List ParentOutMsg`.

Takes care of threading the state. When interpreting an OutMsg changes the model,
the updated model will be used for subsequent interpretations of OutMsgs. Cmds are
accumulated and batched.
-}
evaluateList :
    (childOutMsg -> model -> ( model, Cmd msg, parentOutMsg ))
    -> ( model, Cmd msg, List childOutMsg )
    -> ( model, Cmd msg, List parentOutMsg )
evaluateList interpretOutMsg ( model, cmd, childOutMsgs ) =
    State.traverse (wrap interpretOutMsg) childOutMsgs
        |> State.map batchCmd
        |> run cmd model


{-| Turn a child `Result e OutMsg` into effects, model changes and parent `Result e OutMsg` value.

Has a third argument for a function that turns errors into a command that is
used when child OutMsg is Err error.
-}
evaluateResult :
    (childOutMsg -> model -> ( model, Cmd msg, parentOutMsg ))
    -> (error -> Cmd msg)
    -> ( model, Cmd msg, Result error childOutMsg )
    -> ( model, Cmd msg, Result error parentOutMsg )
evaluateResult interpretOutMsg errDefault ( model, cmd, rChildOutMsg ) =
    case rChildOutMsg of
        Result.Err error ->
            ( model, errDefault error, Result.Err error )

        Result.Ok childOutMsg ->
            let
                ( newModel, newCmd, parentOutMsg ) =
                    evaluate interpretOutMsg ( model, cmd, childOutMsg )
            in
                ( newModel, newCmd, Result.Ok parentOutMsg )


{-| Apply a function over the updated child component.
-}
mapComponent : (childComponent -> a) -> ( childComponent, b, c ) -> ( a, b, c )
mapComponent =
    OutMessage.mapComponent


{-| Apply a function over the Msg from the child.
-}
mapCmd : (childMsg -> parentMsg) -> ( a, Cmd childMsg, c ) -> ( a, Cmd parentMsg, c )
mapCmd =
    OutMessage.mapCmd


{-| Apply a function over the child's OutMsg.
-}
mapOutMsg : (outMsg -> c) -> ( a, b, outMsg ) -> ( a, b, c )
mapOutMsg =
    OutMessage.mapOutMsg



-- Internals


toNestedParentMsg : ( a, b, c ) -> ( a, ( b, c ) )
toNestedParentMsg ( x, y, z ) =
    ( x, ( y, z ) )


fromNestedParentMsg : ( a, ( b, c ) ) -> ( a, b, c )
fromNestedParentMsg ( x, ( y, z ) ) =
    ( x, y, z )


swap : ( a, b ) -> ( b, a )
swap ( x, y ) =
    ( y, x )


batchCmd : List ( Cmd msg, outMsg ) -> ( Cmd msg, List outMsg )
batchCmd elems =
    List.foldl batchElemCmd ( Cmd.none, [] ) elems


batchElemCmd : ( Cmd msg, outMsg ) -> ( Cmd msg, List outMsg ) -> ( Cmd msg, List outMsg )
batchElemCmd ( cmd, outMsg ) ( accCmd, accOutMsg ) =
    ( Cmd.batch [ accCmd, cmd ], accOutMsg ++ [ outMsg ] )


wrap : (childOutMsg -> model -> ( model, Cmd msg, parentOutMsg )) -> childOutMsg -> State model ( Cmd msg, parentOutMsg )
wrap f msg =
    State.advance (swap << toNestedParentMsg << f msg)


run : Cmd msg -> model -> State model ( Cmd msg, parentOutMsg ) -> ( model, Cmd msg, parentOutMsg )
run cmd model =
    -- Prepend the child component's Cmds
    State.map (\( outCmd, parentOutMsg ) -> ( Cmd.batch [ cmd, outCmd ], parentOutMsg ))
        >> State.run model
        >> swap
        >> fromNestedParentMsg
