# Extension to OutMessage for nested components

This package is a simple extension to
[OutMessage](http://package.elm-lang.org/packages/folkertdev/outmessage/latest/)
for nested components, that is scenarios where the child returns a
`ChildOutMsg` and the parent returns a `ParentOutMsg`.

This package is meant to function in exactly the same way as `OutMessage`
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

## Health warning

Having nested child modules is a sign that your Elm application is complex and
potentially over-engineered. There are perfectly valid reasons for doing so
and this package will help you when this is the case. However, you may also
consider re-factoring your application to get rid of the nesting and simplify
your design.

## Thanks

Thanks to [folkertdev](https://github.com/folkertdev) for the great OutMessage
library.
