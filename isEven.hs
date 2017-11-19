module IsEven where
  import Data.Char

  digits :: Int -> [Int]
  digits = map digitToInt . show

  isEven :: Int -> Bool
  isEven x
    | last  (digits x) == 2 = True
    | last  (digits x) == 4 = True
    | last  (digits x) == 6 = True
    | last  (digits x) == 8 = True
    | last  (digits x) == 0 = True
    | otherwise             = False
