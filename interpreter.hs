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

singleCharParser :: (Char -> Bool) -> [Char] -> Result Char
singleCharParser condition (a:as)
  | condition a  = Success a as
  | otherwise    = Failure (a:as)




sequence :: Parser a -> Parser b -> Parser (a,b)
sequence parserA parserB input =
  

{-
 if (parserA input) is a success,
   let Success parsedA remainderA = (parserA input) 
   if (parserB remainderA) is a success,
    let Success parsedB remainderB = (parserB remainderA)
    return Success (parsedA, parsedB) remainderB
   if (parserB remainderA) is a failure,
     return (Failure input)

if (parserA input) is a failure, return (Failure input)


-}

