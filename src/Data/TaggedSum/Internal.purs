module Data.TaggedSum.Internal where

import Prelude

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
class EqRL (rl :: RowList) (r :: # Type) where
  eqRL :: RLProxy rl -> TaggedSum r -> TaggedSum r -> Boolean

instance nilEqRL :: (RowToList r rl, AllVoid rl) => EqRL Nil r where
  eqRL _ = match

instance consEqRL
    :: ( IsSymbol l
       , Eq a
       , Row.Cons l a r_ r
       , Row.Cons l Void r_ r_void
       , EqRL rl r_void
       )
    => EqRL (Cons l a rl) r
  where
    eqRL _ t1 t2 =
        case matching l t1, matching l t2 of
          Left t1_, Left t2_ ->
            eqRL (RLProxy :: RLProxy rl) t1_ t2_
          Right a1, Right a2 ->
            a1 == a2
          _, _ ->
            false
      where
        l :: Prism (TaggedSum r) (TaggedSum r_void) a Void
        l = tag (SProxy :: SProxy l)

instance eqTaggedSum
    :: ( RowToList r rl
       , EqRL rl r
       )
    => Eq (TaggedSum r) where
  eq = eqRL (RLProxy :: RLProxy rl)
