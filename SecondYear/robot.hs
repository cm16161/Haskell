{-import Prelude (hiding Left, Right, Stop, Forward)-}


data Movement x = Stop | Left'(Movement x) | Right'(Movement x) | Forward x (Movement x)

distTrav :: Movement a -> Int
distTrav (Stop ) = 0
distTrav (Left m) = distTrav m
distTrav (Right m) = distTrav m
distTrav (Forward x m) = x + distTrav m
