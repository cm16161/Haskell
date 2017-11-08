-- import Text.Megaparsec
-- import Text.Megaparsec.Char
-- import Text.Megaparsec.Char.Lexer
import Yoda


-- type Parser = Parsec () String

---------------------------------------
--------------------1.1----------------
---------------------------------------

{-

a) string :: String -> Parser String
    Example: parseMaybe (string "Hello") "Hello"

b) many :: f a -> f[a]
    Example: parseMaybe (many(string "is")) "isis"

c) some :: f a -> f[a]
    Example: parseMaybe (some(string "is")) "isis")

d) oneOf :: [Char] -> Parser Char
    Example: parseMaybe (oneOf ("hello")) "h"

e) noneOf :: [Char] -> Parser Char
    Example: parseMaybe (noneOf ("hello")) "w"

f) try :: Parser a -> Parser a
    Example: parseMaybe (try (string "hello")) "hello world"

-}

whitespace :: Parser ()
whitespace = many (oneOf " \t\n\r") *> pure ()

tok :: String -> Parser String
tok ts = whitespace *> string ts <* whitespace

number :: Parser Int
number = read <$> some (oneOf "0123456789")

syntax :: Parser ()
syntax = many (oneOf ",;:\n") *> pure ()

data Robot = Move Int Robot
           | RotateR Robot
           | RotateL Robot
           | Stop
           deriving (Show,Eq)

-- data Robot = Move Int
--            | RotateR
--            | RotateL
--            | Stop
--            deriving (Show,Eq)

str :: String
str = "forward 4 rotate right rotate left forward 20 stop"

str1 :: String
str1 = "forward 4"

-- parseRobot :: Parser Robot

-- parseRobotForward :: Parser Robot
-- parseRobotForward =  Move <$ tok "forward" <*> number
--
-- parseRobotRight :: Parser Robot
-- parseRobotRight = RotateR <$ (tok "rotate" *> tok "right")
--
-- parseRobotLeft :: Parser Robot
-- parseRobotLeft = RotateL <$ (tok "rotate" *> tok "left")
--
-- parseRobotStop :: Parser Robot
-- parseRobotStop = Stop <$ tok "stop"

parseRobot :: Parser Robot
parseRobot = Move <$ tok "forward" <*> number <*> parseRobot
         <|> RotateR <$ (tok "rotate" *> tok "right") <*> parseRobot
         <|> RotateL <$ (tok "rotate" *> tok "left") <*> parseRobot
         <|> Stop <$ tok "stop"
