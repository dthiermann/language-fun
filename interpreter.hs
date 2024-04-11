import Data.Char
import Text.XHtml (input)

-- TODO:
-- tokenize should handle all kinds of chars: newlines, tabs, numbers, symbols
-- write more tests
-- remove unneeded functions

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print (run testTokenize)

-- testing
-- input, expected,
-- f input == expected
-- test f input expected = (f input == expected)
-- f:A -> B, input:A  expected:B
data TestCase a b = TestCase (a -> b) a b
data TestResult b = Passed | Error b b
instance Show b => Show (TestResult b) where
  show Passed = "Passed"
  show (Error output expected) =
    "Failed\n" ++ 
    "Expected: " <> show expected ++ "\n" ++
    "Actual: " <> show output

-- run a single test case
-- if the test passed, just tell us that
-- if the test failed, get the expected and the actual output 
run :: Eq b => TestCase a b -> TestResult b
run (TestCase f input expected) =
  if f input == expected
    then Passed
  else Error (f input) expected

-- tests
testTokenize :: TestCase [Char] (Result [Token] Char)
testTokenize = TestCase tokenize "(define (hello x) ) )"
  (Success [LeftParens, Define, LeftParens, Identifier "hello",
   Identifier "x", RightParens, RightParens, RightParens] [])



-- definitions and functions start with special keywords,
-- applications do not
data Expression = String | Application Expression Expression
 | Function String Expression | Definition Expression Expression

data Token = Identifier String | LeftParens | RightParens | Empty
 | Define | Lambda
  deriving (Eq, Show)

data Result a i = Success a [i] | Failure | EndOfInput
  deriving (Eq, Show)


type Parser a i = [i] -> Result a i

parse :: String -> String
parse input = input

charToToken :: Char -> Token
charToToken '(' = LeftParens
charToToken ')' = RightParens
charToToken '\\' = Lambda
charToToken _   = Empty

stringToToken :: String -> Token
stringToToken "define" = Define
stringToToken name = Identifier name

stringToTokenParser :: String -> Parser Token Char
stringToTokenParser str = mapParser stringToToken (stringParse str)

defineToken :: Parser Token Char
defineToken = stringToTokenParser "define"

charToTokenParser :: Char -> Parser Token Char
charToTokenParser c = mapParser charToToken (charParser c)

leftToken :: Parser Token Char
leftToken = charToTokenParser '('

rightToken :: Parser Token Char
rightToken = charToTokenParser ')'

spaceToken :: Parser Token Char
spaceToken = charToTokenParser ' '

idToken :: Parser Token Char
idToken  = mapParser Identifier wordParser 


parseToken :: Parser Token Char
parseToken = try [defineToken, rightToken, leftToken, spaceToken, idToken]

tokenize :: Parser [Token] Char
tokenize = mapParser removeSpaces (collectNonEmpty parseToken)


removeSpaces :: [Token] -> [Token]
removeSpaces = filter (/= Empty)
-- succeeds if input starts with a letter
letterParser :: Parser Char Char
letterParser = conditionParser isAlpha

-- parse first element of input based on boolean condition
-- fails on empty input
conditionParser :: (i -> Bool) -> Parser i i
conditionParser condition [] = Failure
conditionParser condition (a:as)
  | condition a  = Success a as
  | otherwise    = Failure

-- succeed if first item of input is c, fail otherwise
charParser :: Eq i => i -> Parser i i
charParser c = conditionParser (c ==) 

-- try to run parser a and then parser b
-- if either one fails, the entire parser fails and the parsed remnant is discarded
productParse :: (a -> b -> c) -> Parser a i -> Parser b i -> Parser c i
productParse join p q input =
  case p input of
    Failure -> Failure
    Success parsedP remainderP ->
      case q remainderP of
        Failure -> Failure
        Success parsedQ remainderQ -> Success (join parsedP parsedQ) remainderQ

-- given a string, returns a parser
-- if the string is empty, returns a parser that succeeds on any input
-- if the string is non-empty:
--    parser succeeds if input starts with that string
--    parser fails on empty input
stringParse :: String -> Parser String Char
stringParse "" = Success ""
stringParse (a:as) = productParse (:) (charParser a) (stringParse as)


-- try to run p and then q, and collect the results into a pair
-- always returns a Success
pairParse :: (a -> b -> b) -> b -> Parser a i-> Parser b i-> Parser b i
pairParse join identity p q input = case p input of
  Failure -> Success identity input
  Success parsedP remainderP -> case q remainderP of
    Failure -> Success (join parsedP identity) remainderP
    Success parsedQ remainderQ -> Success (join parsedP parsedQ) remainderQ

{-
run p until it fails,
if p fails the first time, (collectNonEmpty p) fails,
if p succeeds at least once, (collectNonEmpty p) succeeds and returns
all the successful parses in a list
-}
collectNonEmpty :: Parser a i -> Parser [a] i
collectNonEmpty p input =
  case p input of
    Failure -> Failure
    Success parsed rem -> collectAnything p input

-- runs p on the input until p fails
--always returns a Success
--if p fails immediately, return S [] input
--otherwise return a list of the successful parsed pieces and the rem
collectAnything :: Parser a i -> Parser [a] i
collectAnything p input =
  case p input of
    Failure -> Success [] input
    Success parsed rem -> mapResult (parsed:) (collectAnything p rem)

-- applies a function to the output of a successful parse
mapResult :: (a -> b) -> Result a i -> Result b i
mapResult f (Success parsed rem) = Success (f parsed) rem
mapResult f Failure = Failure

-- try to parse leading string of alphabetic chars
-- fails if the input does not begin with an alphabetic char
wordParser :: Parser String Char
wordParser = collectNonEmpty letterParser

-- try to parse with p or q
parallel :: Parser a i-> Parser a i -> Parser a i
parallel p q input =
  case p input of
    Success parsedP remainderP -> Success parsedP remainderP
    Failure -> case q input of
        Success parsedQ remainderQ -> Success parsedQ remainderQ
        Failure -> Failure

-- given a list of parsers, try each one, and return the result
-- of whichever one succeeds first, otherwise fail
-- note that order matters
try :: [Parser a i] -> Parser a i
try = foldr parallel failParser 

failParser :: Parser a i
failParser input = Failure

mapParser :: (a -> b) -> Parser a i-> Parser b i
mapParser f p input = mapResult f (p input)