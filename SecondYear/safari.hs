-- var :: Int -> Int
-- add :: Int -> Int -> Int
--
-- var n = n
-- add x y = x + y
--
--
-- data Expr = Var Int
--   | Add Expr Expr
--
-- eval :: Expr -> Int
-- eval (Var n) = n
-- eval (Add x y) = eval x + eval y
--
--
--
--
--
data Animal = Lion
  | Tiger
  | Gazelle
  | Ant
  deriving (Eq, Show)
--
type ShallSafari = (NumTerrs, NumAnimals)
type NumTerrs = Int
type NumAnimals = Int

territory :: [Animal] -> ShallSafari
territory as = (1, length as)

territory' :: [Animal] -> [Animal]
territory' as = as


data Plot = Territory [Animal] | Quad Plot Plot Plot Plot

territories :: Plot -> Int
territories (Territory _) = 1
territories (Quad p1 p2 p3 p4) = (territories (p1)) + (territories (p2)) + (territories (p3)) + (territories (p4))

maxAn :: Plot -> Int
maxAn (Territory as) = length as
maxAn (Quad p1 p2 p3 p4) =  max (max (max (maxAn (p1)) (maxAn (p2)) )  (maxAn (p3))    ) (maxAn (p4))
--
--
-- Shallow embedding doesnt change, deep embedding does, this is because shallow, we pass in parameters, deep, the data type has to change
-- I.E. data Plot = Territory [as] | Plot Plot Plot
--
--
territory'' :: [Animal] -> [Animal]
territory'' as = as

animalTypes :: Plot -> [Animal]
animalTypes (Territory as) =  as
animalTypes (Quad p1 p2 p3 p4) = animalTypes(p1) ++ animalTypes(p2) ++ animalTypes(p3) ++ animalTypes(p4)

--Bad practice because spelling mistakes can happen, case sensitivity, pattern matching

data PlotC = TerraCore [Animal]
  |Split [(PlotC, Float)]

territoryNo :: PlotC -> Int
territoryNo (TerraCore _) = 1
territoryNo (Split ps) = sum (map (territoryNo) (map fst ps))

maxCore :: PlotC -> Int
maxCore (TerraCore as) = length as
maxCore (Split ps) = maximum (map maxCore (map fst ps))

animalCore :: PlotC -> [Animal]
animalCore (TerraCore as) = as
animalCore (Split ps) =  concat (map animalCore (map fst ps))

quadCore :: PlotC -> PlotC -> PlotC -> PlotC -> PlotC
quadCore p1 p2 p3 p4 = Split [(p1, 1/4), (p2, 1/4), (p3, 1/4), (p4, 1/4)]

triCore :: PlotC -> PlotC -> PlotC -> PlotC
triCore p1 p2 p3 = Split [(p1, 1/3), (p2, 1/3), (p3, 1/3)]



--An advantage of shallow embedding over deep is that it's easier and quicker to implement since syntax is borrowed from the host language and the semantics are directly assigned
--An advantage of deep embedding over shallow is that it's easier to integrate multiple interpretations
--Deeply embedding core language provides multiple semantics easily, since smart constructors created with shallow embedding style, you can add new data types simply by creating a new function
