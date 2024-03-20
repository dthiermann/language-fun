import Data.Char

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print (parse contents)

-- testing
-- input, expected,
-- f input == expected
-- 
data Variable = String
data Expression = Int | Variable | Application Expression Expression
 | Function Variable Expression | Definition Expression Expression

data Token = Word String | LeftParens | RightParens 

data Result a = Success a [Char] | Failure deriving Show


type Parser a = [Char] -> Result a

parse :: String -> String
parse input = input



parseDefinition :: Parser Expression
parseDefinition = productParse (stringParser "define") parseExpression

-- succeeds if input starts with a letter
letterParser :: Parser Char
letterParser = conditionParser isAlpha

-- parse first char of input based on boolean condition
conditionParser :: (Char -> Bool) -> Parser Char
conditionParser condition [] = Failure
conditionParser condition (a:as)
  | condition a  = Success a as
  | otherwise    = Failure

-- succeed if first char of input is c, fail otherwise
charParser :: Char -> Parser Char
charParser c = conditionParser (c ==) 

-- try to run parser a and then parser b
-- if either one fails, the entire parser fails and the parsed remnant is discarded
productParse :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
productParse join p q input =
  case p input of
    Failure -> Failure
    Success parsedP remainderP ->
      case q remainderP of
        Failure -> Failure
        Success parsedQ remainderQ -> Success (join parsedP parsedQ) remainderQ


-- try to run p and then q, and collect the results into a pair
pairParse :: (a -> b -> b) -> b -> Parser a -> Parser b -> Parser b
pairParse join identity p q input = case p input of
  Failure -> Success identity input
  Success parsedP remainderP -> case q remainderP of
    Failure -> Success (join parsedP identity) remainderP
    Success parsedQ remainderQ -> Success (join parsedP parsedQ) remainderQ

-- run p until failure and collect the results
runAndCollect :: Parser a -> Parser [a]
runAndCollect p = pairParse (:) [] p (runAndCollect p)

-- try to parse with parser a or parser b
parallel :: Parser a -> Parser b -> Parser (Either a b)
parallel p q input =
  case p input of
    Success parsedP remainderP -> Success (Left parsedP) remainderP
    Failure -> case q input of
        Success parsedQ remainderQ -> Success (Right parsedQ) remainderQ
        Failure -> Failure

mapParser :: (a -> b) -> Parser a -> Parser b
mapParser f parser input =
  case parser input of
    Success parsed remainder -> Success (f parsed) remainder
    Failure -> Failure

applyParser :: Parser (a -> b) -> Parser a -> Parser b
applyParser parseFunction parseArgument input =
  case parseFunction input of
    Failure -> Failure
    Success f remainder -> case parseArgument remainder of
      Failure -> Failure
      Success argument rest -> Success (f argument) rest

bindParser :: Parser a -> (a -> Parser b) -> Parser b
bindParser p f input = case p input of
  Failure -> Failure
  Success parsed remainder -> f parsed remainder

pureParser :: a -> Parser a
pureParser = Success

stringParser :: String -> Parser String
stringParser "" = Success ""
stringParser (x:xs) = applyParser (mapParser (:) (charParser x)) (stringParser xs)
