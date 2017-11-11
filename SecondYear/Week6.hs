module WorkSheet where
  import Data.Monoid
  import Prelude hiding ((<$), (<*),(*>))

{-
---------------------------
------------1.1-----------
---------------------------
class Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a->b) -> f a -> f b

data Maybe a = Nothing
             | Just a

instance Applicative Maybe where
  pure  :: a -> Maybe a
  pure  = Just
  (<*>) :: Maybe (a->b) -> Maybe a -> Maybe b
  Nothing <*> (Just f) = Nothing
  (Just f) <*> Nothing = Nothing
  (Just f) <*> (Just x) = (Just f x)
  (Nothing) <*> (Nothing) = (Nothing)



-}

  testList :: List Int
  testList = Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Empty))))

  testList' :: List Char
  testList' = Cons 'a' (Cons 'b' (Cons 'c' (Cons 'd' (Cons 'e' Empty))))

  data List a = Empty
              | Cons a (List a)
              deriving Show

  instance Monoid(List a) where
    mempty = Empty
    Empty `mappend` y = y
    Cons x xs `mappend` ys = Cons x(xs<>ys)

  instance Functor List where
    fmap f Empty = Empty
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

  (<$) :: a -> List b -> List a
  x <$ Empty = Empty
  x <$ (Cons y ys) = fmap (const x) (Cons y ys)

    {-  5 <$ [1,2,3]  -> [5,5,5]

    5 <$ Cons 1 (Cons 2 (Cons 3 Empty)) -> Cons 5(Cons 5 (Cons 5 Empty))

    -}

  instance Applicative List where
    -- pure :: a -> List a
    pure x = Cons x Empty
    -- (<*>) :: f (a->b) -> f a -> f b
    -- f <*> Empty = Empty
    (Cons f fs) <*> Empty = Empty
    (Cons f fs) <*>  xs = Cons f y Cons(f ys Empty) <> Cons fs y Cons (fs ys Empty)
