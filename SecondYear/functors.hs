--By applying function g to the base case/identity (Error s), it returns Error s
--By applying function g to (Just a) returns Just (g a), which is funtion g applied to a
-- this follows the functor laws


{-
-------------------------
-- Law 1: fmap id = id --
-------------------------

fmap id (MError s)
= {def fmap}
MError s
= {def id}
id (MError s)

fmap id (MJust a)
= {def fmap}
MJust (id a)
= {def id}
MJust a
= {def id}
id (MJust a)

-----------------------------------------
-- Law 2: fmap (g.f) = fmap g . fmap f --
-----------------------------------------

fmap g . fmap f (MError s)         ||   fmap (g.f) (MError s)
= {def .}                          ||   = {def fmap}
fmap_2 g (fmap_1 f (MError s))   ||     (MError s)
= {def fmap_1}                     ||
fmap_2 g (MError s)              ||
= {def fmap_2}                     ||
(MError s)                       ||

fmap g . fmap f (MJust a)         ||  fmap (g.f) (MJust a)
= {def .}                         ||  = {def fmap}
fmap_2 g (fmap_1 f (MJust a))   ||    MJust (g.f a)
= {def fmap_1}                    ||  = {def .}
fmap_2 g (MJust (f a))          ||    MJust (g (f a))
= {def fmap_2}                    ||
MJust (g (f a))                 ||
-}



data Tree a = Leaf
  | Branch a (Tree a) (Tree a)

instance Functor Tree where
  fmap f Leaf           = Leaf
  fmap f (Branch a l r) =  Branch (f a) (fmap f l) (fmap f r)

sumTree :: Tree Int -> Int
sumTree Leaf = 0
sumTree (Branch a l r) = a + sumTree l + sumTree r


countLeaves :: Tree a -> Int
countLeaves Leaf = 1
countLeaves (Branch a l r) 1 + countLeaves l + countLeaves r
  {-
  -------------------------
  -- Law 1: fmap id = id --
  -------------------------

  fmap id Leaf
  ={def fmap}
  id Leaf
  ={def id}
  Leaf

  -----------------------------------------
  -- Law 2: fmap (g.f) = fmap g . fmap f --
  -----------------------------------------

  fmap g. fmap f Leaf
  = {def .}
  fmap_2 g (fmap_1 f (Leaf))
  = {def fmap_1}
  fmap_2 g (Leaf)
  ={def fmap_2}
  Leaf

  fmap g. fmap f (Branch a l r)
  ={def .}
  fmap_2 g (fmap_1 f (Branch a l r))
  ={def fmap_1}
  fmap_2 g (Branch (f a) (fmap f l) (fmap f r))
  ={def fmap_2}
  (Branch (g.f a) (fmap g.f l) (fmap g.f r))


  Tree: * -> *
  Exception: * -> * -> *
  F: * -> *

  Exception is not a Functor since it requires 2 parameters

  -}

newtype Fix f = In (f (Fix f))
deriving instance (Show(f(Fix f))) => Show(Fix f)

inop :: Fix f -> f(Fix f)
inop (In x) = x

{-
            fmap(cata alg)
  f (Fix f) --------------> f b
  Fx v      unFx ^          v alg
    Fix f ----------------> b
              cata alg
-}

data TreeF a k = LeafF
               | Branch a k k

instance Functor (TreeF a) where
  fmap f LeafF = LeafF
  fmap f (BranchF a l r) = BranchF a (f l) (f r)


sumTreeF :: Fix (TreeF Int) -> Int
sumTreeF = cata alg where
  alg :: TreeF Int Int-> Int
  alg LeafF = 0
  alg (BranchF a l r) = a + l + r

cata :: (Functor f) => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . inop

countLeavesF :: Fix (TreeF a) -> Int
countLeavesF = cata alg where
  alg :: TreeF a Int -> Int
  alg LeafF = 1
  alg (BranchF a l r) = 1 + l + r
