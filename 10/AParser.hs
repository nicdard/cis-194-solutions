{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- representing the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

-- Example parser for strings (whiteSpace separator)
parseString :: Parser String
parseString = Parser f
    where
        f xs
            | null ns   = Nothing
            | otherwise = Just (ns, rest)
            where (ns, rest) = span (\c -> not (isDigit c || isSpace c)) xs


first :: (a -> b) -> (a, c) -> (b, c)
first h (a, c) = (h a, c)

instance Functor Parser where
    fmap h (Parser runParser) = Parser f
        where f s = first h <$> runParser s

instance Applicative Parser where
    -- Consume no input and successfully returns a result f a
    pure a = Parser f
        where f xs = Just (a, xs)
    -- runs p1, passes remaining input to p2 and then applies
    -- the function to the result.
    p1 <*> p2 = Parser f
        where f xs = case runParser p1 xs of
                Nothing         -> Nothing
                Just (h, rest)  -> case runParser p2 rest of
                    Nothing     -> Nothing
                    Just (v, r) -> Just (h v, r)

-- Exercise 3
abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = () <$ abParser


intPair :: Parser [Integer]
intPair = (\x _ -> (:) x) <$> posInt
            <*> char ' '
            <*> ((:[]) <$> posInt)

-- Exercise 4
instance Alternative Parser where
    empty = Parser { runParser = const Nothing }
    p1 <|> p2 = Parser f
        where f xs = runParser p1 xs <|> runParser p2 xs

-- Exercise 5
parseUppercase :: Parser Char
parseUppercase = satisfy isUpper

intOrUppercase :: Parser ()
intOrUppercase = () <$ posInt <|> () <$ parseUppercase


