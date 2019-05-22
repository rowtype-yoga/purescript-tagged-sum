# `purescript-tagged-sum`

A simple row-based variant data type, supporting:

- An Aeson-compatible JSON representation, via `foreign-generic`
- (Polymorphic) traversals via `Prism`s from `profunctor-lenses`

## Example

```text
$ pulp repl

> import Data.Lens (review)
> import Data.Symbol (SProxy(..))
> import Data.TaggedSum (TaggedSum, tag)
> import Foreign.Generic (encodeJSON)

> _foo = SProxy :: SProxy "foo"
> x = review (tag _foo) 42 :: TaggedSum (foo :: Int)
> encodeJSON x
"{\"tag\":\"foo\",\"contents\":42}"
```
