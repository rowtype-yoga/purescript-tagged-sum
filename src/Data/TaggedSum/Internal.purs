module Data.TaggedSum.Internal where

import Prelude

import Data.Maybe (Maybe(..))
import Data.List as L
import Control.Monad.Except (throwError)
import Data.Lens.Prism (Prism, Prism', matching, prism, review)
import Data.Either (Either(..))
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Foreign (F, Foreign, ForeignError(..))
import Foreign.Generic (class Decode, class Encode, decode, encode, defaultOptions)
import Foreign.Generic.Class (decodeWithOptions, encodeWithOptions)
import Prim.Row as Row
import Prim.RowList (class RowToList, kind RowList, Nil, Cons)
import Type.Data.RowList (RLProxy(..))
import Unsafe.Coerce (unsafeCoerce)

foreign import data Any :: Type

-- | A sum type, whose tag is one of the labels in the row `r`,
-- | with a value of the corresponding type.
newtype TaggedSum (r :: # Type) = TaggedSum
  { tag :: String
  , contents :: Any
  }

class TaggedSumEqs (rl :: RowList) where
  taggedSumEqs :: RLProxy rl -> L.List { tag :: String, eq :: (Any -> Any -> Boolean) }

instance eqTaggedSumNil :: TaggedSumEqs Nil where
  taggedSumEqs _ = L.Nil

instance eqTaggedSumCons :: (IsSymbol name, TaggedSumEqs rs, Eq a) => TaggedSumEqs (Cons name a rs) where
  taggedSumEqs _ = L.Cons { tag: reflectSymbol name, eq: coerceEq eq } (taggedSumEqs (RLProxy :: RLProxy rs))
    where
      name = SProxy :: SProxy name
      coerceEq :: (a -> a -> Boolean) -> Any -> Any -> Boolean
      coerceEq = unsafeCoerce

instance eqTaggedSum :: (RowToList r rl, TaggedSumEqs rl) => Eq (TaggedSum r)  where
  eq (TaggedSum a) (TaggedSum b)
    | a.tag == b.tag = 
      let eqs = taggedSumEqs (RLProxy :: RLProxy rl)
      in case L.find ((==) a.tag <<< _.tag) eqs of
        Nothing -> false
        Just {eq:eq'} -> eq' (unsafeCoerce a.contents) (unsafeCoerce b.contents)
    | otherwise = false

-- | Get the tag for a `TaggedSum`.
getTag :: forall r. TaggedSum r -> String
getTag (TaggedSum o) = o.tag

-- | Match a particular tag using a `Prism`.
-- |
-- | See `match`.
case_
  :: forall s t a r
   . Prism s t a Void
  -> (a -> r)
  -> (t -> r)
  -> s
  -> r
case_ p f g s =
  case matching p s of
    Left t -> g t
    Right a -> f a

class AllVoid (rl :: RowList)

instance allVoidNil :: AllVoid Nil
instance allVoidCons :: AllVoid rl => AllVoid (Cons l Void rl)

-- | Pattern match an empty variant, once all possible tags have
-- | been handled using `case_`.
-- |
-- | For example:
-- |
-- | ```purescript
-- | x :: TaggedSum (foo :: Int, bar :: Char)
-- | x = review (tag (SProxy :: SProxy "foo")) 42
-- |
-- | f :: TaggedSum (foo :: Int, bar :: Char) -> String
-- | f = match
-- |   # case_ (tag (SProxy :: SProxy "foo")) show
-- |   # case_ (tag (SProxy :: SProxy "bar")) show
-- |
-- | > f x
-- | "42"
-- | ```
match
  :: forall r rl x
   . RowToList r rl
  => AllVoid rl
  => TaggedSum r
  -> x
match = absurd <<< unsafeCoerce

-- | Create a `Prism` for accessing a specific tag.
tag
  :: forall tag s t a b r_
   . IsSymbol tag
  => Row.Cons tag a r_ s
  => Row.Cons tag b r_ t
  => SProxy tag
  -> Prism (TaggedSum s) (TaggedSum t) a b
tag t = prism ctor unctor where
  ctor :: b -> TaggedSum t
  ctor value = TaggedSum
    { tag: reflectSymbol t
    , contents: unsafeCoerce value
    }

  unctor :: TaggedSum s -> Either (TaggedSum t) a
  unctor (TaggedSum o) =
    if o.tag == reflectSymbol t
      then Right (unsafeCoerce o.contents)
      else Left (TaggedSum o)

-- | A simpler, non-type changing `Prism`, which might
-- | make type inference possible in some cases.
tag_
  :: forall tag a r r_
   . IsSymbol tag
  => Row.Cons tag a r_ r
  => SProxy tag
  -> Prism' (TaggedSum r) a
tag_ t = tag t

-- | Create a `Prism` for accessing the complement of
-- | a specific tag.
cotag
  :: forall tag s t s_ t_ a
   . IsSymbol tag
  => Row.Cons tag a s_ s
  => Row.Cons tag a t_ t
  => SProxy tag
  -> Prism (TaggedSum s) (TaggedSum t) (TaggedSum s_) (TaggedSum t_)
cotag t = prism ctor unctor where
  ctor :: TaggedSum t_ -> TaggedSum t
  ctor = unsafeCoerce

  unctor :: TaggedSum s -> Either (TaggedSum t) (TaggedSum s_)
  unctor (TaggedSum o) =
    if o.tag == reflectSymbol t
      then Left (unsafeCoerce o)
      else Right (unsafeCoerce o)

-- | A simpler, non-type changing `Prism`, which might
-- | make type inference possible in some cases.
cotag_
  :: forall tag a r r_
   . IsSymbol tag
  => Row.Cons tag a r_ r
  => SProxy tag
  -> Prism' (TaggedSum r) (TaggedSum r_)
cotag_ t = cotag t

instance decodeTaggedSum :: (RowToList r rl, DecodeHelper r rl) => Decode (TaggedSum r) where
  decode = decodeHelper (RLProxy :: RLProxy rl) <=< decodeWithOptions defaultOptions

instance encodeTaggedSum :: (RowToList r rl, EncodeHelper r rl) => Encode (TaggedSum r) where
  encode = encodeWithOptions defaultOptions <<< encodeHelper (RLProxy :: RLProxy rl)

class DecodeHelper (r :: # Type) (rl :: RowList) | rl -> r where
  decodeHelper
    :: RLProxy rl
    -> { tag :: String, contents :: Foreign }
    -> F (TaggedSum r)

instance decodeHelperNil :: DecodeHelper () Nil where
  decodeHelper _ _ = throwError (pure (ForeignError "Unknown tag"))

instance decodeHelperCons
    :: ( IsSymbol l
       , DecodeHelper r_ rl
       , Decode a
       , Row.Cons l a r_ r
       )
    => DecodeHelper r (Cons l a rl)
  where
    decodeHelper _ f =
        if f.tag == reflectSymbol (SProxy :: SProxy l)
          then review p <$> decode f.contents
          else unsafeExpand <$> decodeHelper rl f
      where
          l = SProxy :: SProxy l
          rl = RLProxy :: RLProxy rl

          p = tag_ l

          unsafeExpand :: TaggedSum r_ -> TaggedSum r
          unsafeExpand = unsafeCoerce

class EncodeHelper (r :: # Type) (rl :: RowList) | rl -> r where
  encodeHelper
    :: RLProxy rl
    -> TaggedSum r
    -> { tag :: String, contents :: Foreign }

instance encodeHelperNil :: (RowToList r rl, AllVoid rl) => EncodeHelper r Nil where
  encodeHelper _ = match

instance encodeHelperCons
    :: ( IsSymbol l
       , EncodeHelper r__ rl
       , Encode a
       , Row.Cons l a r_ r
       , Row.Cons l Void r_ r__
       )
    => EncodeHelper r (Cons l a rl)
  where
    encodeHelper opts = case_ (tag l) here (encodeHelper rl) where
      l = SProxy :: SProxy l
      rl = RLProxy :: RLProxy rl

      here a =
        { tag: reflectSymbol l
        , contents: encode a
        }
