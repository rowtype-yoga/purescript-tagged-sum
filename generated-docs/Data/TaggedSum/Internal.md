## Module Data.TaggedSum.Internal

#### `Any`

``` purescript
data Any :: Type
```

#### `TaggedSum`

``` purescript
newtype TaggedSum (r :: # Type)
  = TaggedSum { tag :: String, contents :: Any }
```

A sum type, whose tag is one of the labels in the row `r`,
with a value of the corresponding type.

##### Instances
``` purescript
(RowToList r rl, DecodeHelper r rl) => Decode (TaggedSum r)
(RowToList r rl, EncodeHelper r rl) => Encode (TaggedSum r)
```

#### `getTag`

``` purescript
getTag :: forall r. TaggedSum r -> String
```

Get the tag for a `TaggedSum`.

#### `case_`

``` purescript
case_ :: forall s t a r. Prism s t a Void -> (a -> r) -> (t -> r) -> s -> r
```

Match a particular tag using a `Prism`.

See `match`.

#### `AllVoid`

``` purescript
class AllVoid (rl :: RowList) 
```

##### Instances
``` purescript
AllVoid Nil
(AllVoid rl) => AllVoid (Cons l Void rl)
```

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

#### `tag`

``` purescript
tag :: forall tag s t a b r_. IsSymbol tag => Cons tag a r_ s => Cons tag b r_ t => SProxy tag -> Prism (TaggedSum s) (TaggedSum t) a b
```

Create a `Prism` for accessing a specific tag.

#### `tag_`

``` purescript
tag_ :: forall tag a r r_. IsSymbol tag => Cons tag a r_ r => SProxy tag -> Prism' (TaggedSum r) a
```

A simpler, non-type changing `Prism`, which might
make type inference possible in some cases.

#### `cotag`

``` purescript
cotag :: forall tag s t s_ t_ a. IsSymbol tag => Cons tag a s_ s => Cons tag a t_ t => SProxy tag -> Prism (TaggedSum s) (TaggedSum t) (TaggedSum s_) (TaggedSum t_)
```

Create a `Prism` for accessing the complement of
a specific tag.

#### `cotag_`

``` purescript
cotag_ :: forall tag a r r_. IsSymbol tag => Cons tag a r_ r => SProxy tag -> Prism' (TaggedSum r) (TaggedSum r_)
```

A simpler, non-type changing `Prism`, which might
make type inference possible in some cases.

#### `DecodeHelper`

``` purescript
class DecodeHelper (r :: # Type) (rl :: RowList) | rl -> r where
  decodeHelper :: RLProxy rl -> { tag :: String, contents :: Foreign } -> F (TaggedSum r)
```

##### Instances
``` purescript
DecodeHelper () Nil
(IsSymbol l, DecodeHelper r_ rl, Decode a, Cons l a r_ r) => DecodeHelper r (Cons l a rl)
```

#### `EncodeHelper`

``` purescript
class EncodeHelper (r :: # Type) (rl :: RowList) | rl -> r where
  encodeHelper :: RLProxy rl -> TaggedSum r -> { tag :: String, contents :: Foreign }
```

##### Instances
``` purescript
(RowToList r rl, AllVoid rl) => EncodeHelper r Nil
(IsSymbol l, EncodeHelper r__ rl, Encode a, Cons l a r_ r, Cons l Void r_ r__) => EncodeHelper r (Cons l a rl)
```


