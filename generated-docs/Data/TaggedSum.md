## Module Data.TaggedSum

A variant data type, based on rows, but unlike
`purescript-variant`, this one uses a representation
which is compatible with Aeson, so that we can
use the data we get from the server directly.


### Re-exported from Data.TaggedSum.Internal:

#### `TaggedSum`

``` purescript
newtype TaggedSum (r :: # Type)
```

A sum type, whose tag is one of the labels in the row `r`,
with a value of the corresponding type.

##### Instances
``` purescript
(RowToList r rl, DecodeHelper r rl) => Decode (TaggedSum r)
(RowToList r rl, EncodeHelper r rl) => Encode (TaggedSum r)
```

#### `tag_`

``` purescript
tag_ :: forall tag a r r_. IsSymbol tag => Cons tag a r_ r => SProxy tag -> Prism' (TaggedSum r) a
```

A simpler, non-type changing `Prism`, which might
make type inference possible in some cases.

#### `tag`

``` purescript
tag :: forall tag s t a b r_. IsSymbol tag => Cons tag a r_ s => Cons tag b r_ t => SProxy tag -> Prism (TaggedSum s) (TaggedSum t) a b
```

Create a `Prism` for accessing a specific tag.

#### `match`

``` purescript
match :: forall r rl x. RowToList r rl => AllVoid rl => TaggedSum r -> x
```

Pattern match an empty variant, once all possible tags have
been handled using `case_`.

For example:

```purescript
x :: TaggedSum (foo :: Int, bar :: Char)
x = review (tag (SProxy :: SProxy "foo")) 42

f :: TaggedSum (foo :: Int, bar :: Char) -> String
f = match
  # case_ (tag (SProxy :: SProxy "foo")) show
  # case_ (tag (SProxy :: SProxy "bar")) show

> f x
"42"
```

#### `getTag`

``` purescript
getTag :: forall r. TaggedSum r -> String
```

Get the tag for a `TaggedSum`.

#### `cotag_`

``` purescript
cotag_ :: forall tag a r r_. IsSymbol tag => Cons tag a r_ r => SProxy tag -> Prism' (TaggedSum r) (TaggedSum r_)
```

A simpler, non-type changing `Prism`, which might
make type inference possible in some cases.

#### `cotag`

``` purescript
cotag :: forall tag s t s_ t_ a. IsSymbol tag => Cons tag a s_ s => Cons tag a t_ t => SProxy tag -> Prism (TaggedSum s) (TaggedSum t) (TaggedSum s_) (TaggedSum t_)
```

Create a `Prism` for accessing the complement of
a specific tag.

#### `case_`

``` purescript
case_ :: forall s t a r. Prism s t a Void -> (a -> r) -> (t -> r) -> s -> r
```

Match a particular tag using a `Prism`.

See `match`.

