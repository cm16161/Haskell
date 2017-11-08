{-# LANGUAGE RankNTypes #-}

---------------------
-----SECTION 1-------
---------------------

---------------------
-----SECTION 1.1-----
---------------------


newtype Fix f = In (f (Fix f))


---------------------
-----SECTION 1.2-------
---------------------
inop :: Fix f -> f(Fix f)
inop (In x) = x

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . inop

---------------------
-----SECTION 2-------
---------------------


---------------------
-----SECTION 2.1-----
---------------------

data Robot k = Move Int k
  | RotateR k
  | RotateL k
  | Stop

  ---------------------
  -----SECTION 2.2-----
  ---------------------

instance Functor Robot where
  fmap _ Stop        = Stop
  fmap f (RotateR r) = RotateR (f r)
  fmap f (RotateL r) = RotateL (f r)
  fmap f (Move d r)  = Move d (f r)

  ---------------------
  -----SECTION 2.3-----
  ---------------------

{-
-------------------------
-- Law 1: fmap id = id --
-------------------------

fmap id Stop
={def fmap}
id Stop
={def id}
Stop


-----------------------------------------
-- Law 2: fmap (g.f) = fmap g . fmap f --
-----------------------------------------

fmap g . fmap f Stop
= {def .}
fmap_2 g  (fmap_1 f (Stop))
= {def fmap}
fmap_2 g (Stop)
= {def fmap_2}
Stop

fmap g . fmap f (RotateR r)
= {def .}
fmap g (fmap f (RotateR r))
= {def fmap}
fmap g (RotateR f r)
= {def fmap}
RotateR g . f r

fmap g . fmap f (RotateL r)
= {def .}
fmap g (fmap f (RotateL r))
= {def fmap}
fmap g (RotateL (f r))
= {def fmap}
RotateL (g . f r)

fmap g . fmap f (Move d r)
= {def .}
fmap g (fmap f (Move d r))
= {def fmap}
fmap g (Move d (f r))
= {def fmap}
Move d (g . f r)


-}

---------------------
-----SECTION 2.4-----
---------------------

dist :: Fix Robot -> Int
dist = cata alg where
  alg :: Robot Int -> Int
  alg Stop        = 0
  alg (RotateL r) = r
  alg (RotateR r) = r
  alg (Move d r)  = d + r


--alg :: f a -> a
--alg :: f (a,a)

distFacing :: Fix Robot -> Int
distFacing x = fst(cata alg x) where
  alg Stop             = (0,0)
  alg (RotateL (v, c)) = (v,c-1)
  alg (RotateL (v, c)) = (v,c+1)
  alg (Move i (j,c))   = if c == 0 then (i+j,c) else (j, c)
  -- alg Move d r =


fir :: (a,a,a) -> a
fir (x,y,z) = x

thr :: (a,a,a) -> a
thr (x,y,z) = z

triangulate :: Int -> Int -> Float
triangulate x y = sqrt(fromIntegral (x^2) + fromIntegral (y^2))

distLine :: Fix Robot -> Float
distLine x = triangulate (fir (cata algLine x)) (thr (cata algLine x)) where
  algLine (RotateL (v,c, hrz)) = (v, c-1, hrz)
  algLine (RotateR (v,c, hrz)) = (v, c+1, hrz)
  algLine Stop = (0,0,0)
  algLine (Move i (j,c,hrz))
    | c == 0 = (i +j, c,hrz)
    | c == -1 = (j,c,hrz -i)
    | c > 0 = (j, c, hrz +i)



  ---------------------
  -----SECTION 2.7-----
  ---------------------
{-

b.fmap h. fmap(cata.a). inop

-}


---------------------
-----SECTION 3.1-----
---------------------


mcata :: Functor f => (forall x . (x -> a) -> (f x -> a)) -> Fix f -> a
mcata phi = phi (mcata phi) .inop

---------------------
-----SECTION 3.2-----
---------------------

data List a x = Empty
  | Cons a x

instance Functor (List a) where
  fmap f Empty       = Empty
  fmap f (Cons x xs) = Cons x (f xs)



lenalg :: (x->Int) -> (List b x -> Int)
lenalg g Empty       = 0
lenalg g (Cons x xs) = g xs + 1

sumalg :: (x -> Int) -> (List Int x -> Int)
sumalg g Empty       = 0
sumalg g (Cons x xs) = g xs + x

giveLength :: Fix (List a) -> Int
giveLength = mcata alg where
  alg = lenalg

giveSum :: Fix (List Int) -> Int
giveSum = mcata alg where
  alg = sumalg
