# Extension to OutMessage for nested components

This package is a simple extension to
[OutMessage](http://package.elm-lang.org/packages/folkertdev/outmessage/latest/)
for nested components, that is scenarios where the chhild returns an
`ChildOutMsg` and the parent returns a `ParentOutMsg`.

This package is meant to function in exactlye the same way as `OutMessage`
while handling the transformation of child message to parent message:

```elm
-- in update : Msg -> Model -> (Model, Cmd Msg, ParentOutMsg)
-- assuming interpretOutMsg : ChildOutMsg -> Model -> (Model, Cmd Msg, ParentOutMsg)
ChildComponentMessageWrapper childMsg ->
    ChildComponentModule.update childMsg model.child
        -- update the model with the new child component
        |> OutMessage.Nested.mapComponent
            (\newChild -> { model | child = newChild }
        -- convert child cmd to parent cmd
        |> OutMessage.Nested.mapCmd ChildComponentMessageWrapper
        -- apply outmsg changes
        |> OutMessage.Nested.evaluate interpretOutMsg
```

# Thanks

Thanks to [folkertdev](https://github.com/folkertdev) for the great OutMessage
library.
