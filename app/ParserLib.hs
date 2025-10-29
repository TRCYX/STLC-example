module ParserLib
  ( Parser(parse)
  , keyword
  , ident
  , symbol
  , natural
  , noMoreTokens
  , parens
  )
where

import Data.Char (isSpace, isLetter, isDigit, isSymbol)
import Data.Functor (void)
import Control.Applicative (Alternative(..))

-- * The `Parser` type and its class instances

newtype Parser a = P { parse :: String -> [(a, String)] }

instance Functor Parser where
  -- fmap :: (a -> b) -> (Parser a -> Parser b)
  fmap f pa = P $ \inp -> [ (f a, s) | (a, s) <- parse pa inp ]

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure a = P $ \inp -> [(a, inp)]

  -- At least either (<*>) or liftA2 should be given

  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> pa = P $ \inp -> [ (f a, s')
                          | (f, s ) <- parse pf inp
                          , (a, s') <- parse pa s
                          ]

  -- liftA2 :: (a -> b -> c) -> (Parser a -> Parser b -> Parser c)
  liftA2 f pa pb = P $ \inp -> [ (f a b, s')
                               | (a, s ) <- parse pa inp
                               , (b, s') <- parse pb s
                               ]

instance Monad Parser where
  -- This is automatically provided:
  -- return :: a -> Parser a
  -- return = pure

  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  pa >>= k = P $ \inp -> [ (b, s')
                         | (a, s ) <- parse pa inp
                         , (b, s') <- parse (k a) s
                         ]

instance Alternative Parser where
  -- empty :: Parser a
  empty = P $ \_inp -> []

  -- (<|>) :: Parser a -> Parser a -> Parser a
  pa <|> pa' = P $ \inp -> parse pa inp ++ parse pa' inp

  -- These are automatically provided (but a bit more performant):
  -- some :: Parser a -> Parser [a]
  -- some pa = (:) <$> pa <*> some pa

  -- many :: Parser a -> Parser [a]
  -- many pa = some pa <|> pure []

-- * Basic `Parser`s

item :: Parser Char
item = P $ \inp -> case inp of
                     []     -> []
                     (c:cs) -> [(c, cs)]

sat :: (Char -> Bool) -> Parser Char
sat pred = do
  c <- item
  if pred c then pure c else empty

char :: Char -> Parser Char
char c = sat (c ==)

string :: String -> Parser String
string []     = pure []
string (c:cs) = (:) <$> char c <*> string cs

space :: Parser ()
space = void $ many (sat isSpace)

-- | End-of-file
eof :: Parser ()
eof = P $ \inp -> case inp of
                    [] -> [((), inp)]
                    _  -> []

-- | Returns a `()` if we reach the end of input, or the next char does not satisfy `pred`.
nextNot :: (Char -> Bool) -> Parser ()
nextNot pred = P $ \inp -> case inp of
                             []                    -> [((), inp)]
                             (c:_) | not (pred c) -> [((), inp)]
                                   | otherwise    -> empty

-- * Tokenization

-- Space after the token is not consumed.

isLetterOrDigit :: Char -> Bool
isLetterOrDigit c = isLetter c || isDigit c
-- Or by `Applicative` of `(->)`:
-- isLetterOrDigit = liftA2 (||) isLetter isDigit

keyword :: String -> Parser ()
keyword kw = do
  space
  _ <- string kw
  nextNot isLetterOrDigit
  return ()

ident :: Parser String
ident = do
  space
  h <- sat isLetter
  t <- many (sat isLetterOrDigit)
  nextNot isLetterOrDigit
  return $ h:t

symbol :: String -> Parser ()
symbol s = do
  space
  _ <- string s
  nextNot (\c -> isSymbol c && c /= '(' && c /= ')')
  return ()

natural ::  Parser Int
natural = do
  space
  s <- some (sat isDigit)
  nextNot isDigit
  return $ read s

noMoreTokens :: Parser ()
noMoreTokens = do
  space
  eof

-- | An arbitrary parser in parentheses. 
parens :: Parser a -> Parser a
parens pa = do
  space
  _ <- char '('
  a <- pa
  space
  _ <- char ')'
  return a
