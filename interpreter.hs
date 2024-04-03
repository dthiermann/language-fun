import Data.Char

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print (parse contents)

-- testing
-- input, expected,
-- f input == expected
-- 
data Expression = String | Application Expression Expression
 | Function String Expression | Definition Expression Expression

data Token = Identifier String | LeftParens | RightParens | WhiteSpace

data Result a = Success a [Char] | Failure deriving Show

data Tree a = Leaf a | Tree (Tree a) (Tree a)

type Parser a = [Char] -> Result a

parse :: String -> String
parse input = input

-- remove leading spaces from string
acceptWhiteSpace :: Parser Char
acceptWhiteSpace "" = Failure
acceptWhiteSpace (' ':as) = Success ' ' as
acceptWhiteSpace otherInput = Failure


leftToken :: Parser Token
leftToken ('(':rest) = Success LeftParens rest
leftToken _ = Failure

rightToken :: Parser Token
rightToken (')':rest) = Success RightParens rest
rightToken _ = Failure

spaceToken :: Parser Token
spaceToken (' ':rest) = Success WhiteSpace rest
spaceToken _ = Failure

idToken :: Parser Token
idToken = mapParser Identifier wordParser

tokenize :: Parser [Token]
tokenize = runAndCollect (try [rightToken, leftToken, spaceToken, idToken])


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

stringParse :: String -> Parser String
stringParse "" = Success ""
stringParse (a:as) = productParse (:) (charParser a) (stringParse as)


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

-- try to parse leading word
wordParser :: Parser String
wordParser = runAndCollect (conditionParser isAlpha)

-- try to parse with p or q
parallel :: Parser a -> Parser a -> Parser a
parallel p q input =
  case p input of
    Success parsedP remainderP -> Success parsedP remainderP
    Failure -> case q input of
        Success parsedQ remainderQ -> Success parsedQ remainderQ
        Failure -> Failure

-- given a list of parsers, try each one, and return the result
-- of whichever one succeeds first, otherwise fail
try :: [Parser a] -> Parser a
try = foldr parallel failParser 

failParser :: Parser a
failParser input = Failure

mapParser :: (a -> b) -> Parser a -> Parser b
mapParser f parser input =
  case parser input of
    Success parsed remainder -> Success (f parsed) remainder
    Failure -> Failure

applyParser :: Parser (a -> b) -> Parser a -> Parser b
applyParser parseFunction parseArgument input =
  case parseFunction input of
    Failure -> Failure
    Success f remainder ->
      case parseArgument remainder of
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

{-
tokenize:
ignore whitespace,
parse ( and )
parse words

a b c d . . .




Word is a list of letters (non-whitespace, non-parens)
space-sep-list expr = list of exprs separated by spaces

expr = Word | space-sep-list expr | leftParens + expr + rightParens



-}

