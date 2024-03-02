import Data.Char

main :: IO ()
main = putStrLn "Hello"

data Variable = String
data Expression = Int | Variable | Application Expression Expression
 | Function Variable Expression

data Token = Word String | LeftParens | RightParens 

-- Success parsed remainder
-- Failure input
data Result a = Success a [Char] | Failure [Char] | EndOfFile

type Parser a = [Char] -> Result a

singleCharParser :: (Char -> Bool) -> Parser Char
singleCharParser condition (a:as)
  | condition a  = Success a as
  | otherwise    = Failure (a:as)

sequence :: Parser a -> Parser b -> Parser (a,b)
sequence p q input =
  case p input of
    Failure remainder -> Failure remainder
    Success parsedP remainderP ->
      case q remainderP of
        Failure remainderQ -> Failure input
        Success parsedQ remainderQ -> Success (parsedP, parsedQ) remainderQ

-- try to use sequence to write a parser for strings of alpha-chars


  

{-


-}

