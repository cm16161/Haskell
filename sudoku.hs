{-
Preamble
========

The following lines define this module and import some useful
functions and definitions.
-}

module Sudoku where
import System.Environment
import Data.List



-- Here we define the type called `Sudoku`, and the synonym `Cell`.
type Cell = [Char]
data Sudoku = Sudoku [Cell]


{-
Tasks
=====

The tasks for you to complete are below:
-}

-- TODO #S1:
bundle :: Int -> [a] -> [[a]]
bundle = undefined

-- TODO #S2
stuck :: Sudoku -> Bool
stuck (Sudoku cells) = undefined


-- TODO #S3
fixed :: Sudoku -> Bool
fixed (Sudoku cells) = undefined

-- TODO #S4a:
rows :: Sudoku -> [[Cell]]
rows (Sudoku cells) = undefined

-- TODO #S4b:
cols :: Sudoku -> [[Cell]]
cols (Sudoku cells) = undefined

-- TODO #S4c:
boxs :: Sudoku -> [[Cell]]
boxs (Sudoku cells) = undefined


{-
Puzzles
=======

Here are some puzzles for you to play with.
-}

broken :: String
broken = (concat . concat . replicate 3)
  [ concat (replicate 3 ['1' .. '3'])
  , concat (replicate 3 ['4' .. '6'])
  , concat (replicate 3 ['7' .. '9']) ]

solved :: String
solved = concat
    [ simpleRow 1
    , simpleRow 4
    , simpleRow 7
    , simpleRow 2
    , simpleRow 5
    , simpleRow 8
    , simpleRow 3
    , simpleRow 6
    , simpleRow 9 ]
  where
    simpleRow n = take 9 (drop (n-1) (cycle ['1' .. '9']))

easy :: String
easy = " 4   2 19   351 8631  947   94     7         2     89   952  4142 169   16 8   7 "

-- Apparently the next one is the hardest Sudoku to solve, at least, according to:
-- "http://www.telegraph.co.uk/news/science/science-news/9359579/Worlds-hardest-sudoku-can-you-crack-it.html"
hard:: String
hard = "8          36      7  9 2   5   7       457     1   3   1    68  85   1  9    4  "


{-
Appendix
========

This appendix contains code that you need not modify. You do not need
to understand how this work at this point in the course, but you are
welcome to have a look.
-}

lone :: [a] -> Bool
lone [x] = True
lone xs  = False

solve :: Sudoku -> [Sudoku]
solve puzzle
  | stuck puzzle       = []
  | not (valid puzzle) = []
  | fixed puzzle       = [puzzle]
  | otherwise          = [puzzle'' | puzzle'  <- expand puzzle
                                   , puzzle'' <- solve  puzzle']

unique :: Eq a => [a] -> Bool
unique xs = nub xs == xs

valid :: Sudoku -> Bool
valid puzzle = all (unique . filter lone) (rows puzzle)
            && all (unique . filter lone) (cols puzzle)
            && all (unique . filter lone) (boxs puzzle)

expand :: Sudoku -> [Sudoku]
expand (Sudoku cells) = [ Sudoku (ls ++ [choice] : rs) | choice <- choices ]
  where
    (ls, choices:rs) = break (not . lone) cells

main :: IO ()
main = do
  args <- getArgs
  puzzle <- case args of
                 [fileName] -> readFile fileName
                 []         -> return hard
  print (solve (sudoku puzzle))

sudoku :: String -> Sudoku
sudoku xs
  | length ys == 81 = Sudoku (map convert ys)
  | otherwise       = error ("Invalid sudoku puzzle: " ++ show xs)
  where convert x = if blank x then ['1'..'9'] else [x]
        newline c = c == '\n'  || c == '\r'
        blank c = c == ' ' || c == '.'
        ys = filter (not . newline) xs

instance Show Sudoku where
  show (Sudoku xs) = "\n" ++
      top ++
      intercalate mid
        [intercalate sep
          [row (xss !! (i + (j*3))) | i <- [0 .. 2] ] | j <- [0 .. 2]] ++
      bot
    where
     row :: [String] -> String
     top    = 'â•”' : intercalate "â•¦" (replicate 3 (intercalate "â•¤" (replicate 3 "â•â•â•"))) ++ "â•—\n"
     row rs = 'â•‘' : intercalate "â•‘" (map (intercalate "â”‚") (bundle 3 rs)) ++ "â•‘\n"
     mid    = 'â• ' : intercalate "â•¬" (replicate 3 (intercalate "â•ª" (replicate 3 "â•â•â•"))) ++ "â•£\n"
     sep    = 'â•Ÿ' : intercalate "â•«" (replicate 3 (intercalate "â”¼" (replicate 3 "â”€â”€â”€"))) ++ "â•¢\n"
     bot    = 'â•š' : intercalate "â•©" (replicate 3 (intercalate "â•§" (replicate 3 "â•â•â•"))) ++ "â•\n"
     xss = bundle 9 ys
     ys  = map summary xs
     summary [x] = " " ++ [x] ++ " "
     summary _   = "   "
