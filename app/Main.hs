module Main where

import           Control.Applicative (Alternative (..))
import           Data.Foldable
import           ParserLib
import           System.Environment  (getArgs)

data Term
  = TmVar String           -- ^ Variables
  | TmAbs String Type Term -- ^ Lambdas
  | TmApp Term Term        -- ^ Function applications
  | TmInt Int              -- ^ Integer Literals
  | TmPlus Term Term       -- ^ A sample binary operator

data Type
  = TyInt           -- ^ Int type
  | TyArr Type Type -- ^ Function type
  deriving Eq

instance Show Term where
  showsPrec _ (TmVar x) = showString x
  showsPrec p (TmAbs x ty tm) = showParen (p > 0) $
    showChar '\\' . showString x . showString " :: " . showsPrec 11 ty . showString " -> " . showsPrec 0 tm
  showsPrec p (TmApp tm1 tm2) = showParen (p > 10) $
    showsPrec 10 tm1 . showChar ' ' . showsPrec 11 tm2
  showsPrec _ (TmInt i) = shows i
  showsPrec p (TmPlus tm1 tm2) = showParen (p > 6) $
    showsPrec 6 tm1 . showString " + " . showsPrec 7 tm2

instance Show Type where
  showsPrec _ TyInt         = showString "Int"
  showsPrec p (TyArr t1 t2) = showParen (p > 0) $
    showsPrec 1 t1 . showString " -> " . showsPrec 0 t2

parseTerm :: Parser Term
parseTerm = parseSumTerm
        <|> do
              symbol "\\"
              x <- ident
              symbol "::"
              ty <- parseType
              symbol "->"
              tm <- parseTerm
              return $ TmAbs x ty tm
  where
    parseSumTerm :: Parser Term
    parseSumTerm = do
      first <- parseAppTerm
      rest <- many (symbol "+" >> parseAppTerm)
      return $ foldl' TmPlus first rest

    parseAppTerm :: Parser Term
    parseAppTerm = do
      first <- parseAtomTerm
      rest <- many parseAtomTerm
      return $ foldl' TmApp first rest

    parseAtomTerm :: Parser Term
    parseAtomTerm = parens parseTerm
                <|> TmInt <$> natural
                <|> TmVar <$> ident

parseType :: Parser Type
parseType = do
    first <- parseAtomType
    (do
      symbol "->"
      rest <- parseType
      return $ TyArr first rest)
     <|> pure first
  where
    parseAtomType :: Parser Type
    parseAtomType = parens parseType
                <|> TyInt <$ keyword "Int"

parseLang :: String -> Maybe Term
parseLang inp =
  case parse (parseTerm <* noMoreTokens) inp of
    []          -> Nothing
    ((tm, _):_) -> Just tm

type Context = [(String, Type)]

newtype TC a = TC { runTC :: Context -> Either String a }

-- The instance should be derived with monad transformers!
-- Without them, we can give very abstract instances if we do not want to dive into the details of `(->)` and `Either`

instance Functor TC where
  -- fmap :: (a -> b) -> (TC a -> TC b)
  fmap f = TC . fmap (fmap f) . runTC
             -- ^ for `(->) Context`
                   -- ^ for `Either String`

instance Applicative TC where
  -- pure :: a -> TC a
  pure = TC . pure . pure
           -- ^ for `(->) Context`
                  -- ^ for `Either String`

  -- liftA2 :: (a -> b -> c) -> (TC a -> TC b -> TC c)
  liftA2 f (TC ma) (TC mb) = TC $ liftA2 (liftA2 f) ma mb
                               -- ^ for `(->) Context`
                                       -- ^ for `Either String`

instance Monad TC where
  -- (>>=) :: TC a -> (a -> TC b) -> TC b
  (TC ma) >>= f = TC $ liftA2 (>>=) ma (flip (runTC . f))
                    -- ^ for `(->) Context`
                            -- ^ for `Either String

with :: String -> Type -> TC a -> TC a
with x ty ma = TC $ \ctx -> runTC ma ((x, ty):ctx)

reportError :: String -> TC a
reportError msg = TC $ \_ -> Left msg

lookupVar :: String -> TC Type
lookupVar x = TC go
  where
    go :: Context -> Either String Type
    go []                        = Left $ "Unbound variable: " ++ x
    go ((y, ty):ctx) | x == y    = Right ty
                     | otherwise = go ctx

typeCheck :: Term -> TC Type
typeCheck (TmVar x)        = lookupVar x
typeCheck (TmAbs x ty tm)  = TyArr ty <$> with x ty (typeCheck tm)
typeCheck (TmApp fun arg)  = do
  tyfun <- typeCheck fun
  tyarg <- typeCheck arg
  case tyfun of
    TyArr typar tyret
      | typar == tyarg -> pure tyret
      | otherwise      -> reportError $ "Expected term of type " ++ show typar ++ ", found " ++ show arg ++ " of type " ++ show tyarg
    _ -> reportError $ "Applying non-function " ++ show fun ++ " of type " ++ show tyfun
typeCheck (TmInt _)        = pure TyInt
typeCheck (TmPlus tm1 tm2) = do
  ty1 <- typeCheck tm1
  ty2 <- typeCheck tm2
  case (ty1, ty2) of
    (TyInt, TyInt) -> pure TyInt
    (TyInt, _)     -> reportError $ "Expected operand of type Int, found " ++ show tm2 ++ " of type " ++ show ty2
    (_    , _)     -> reportError $ "Expected operand of type Int, found " ++ show tm1 ++ " of type " ++ show ty1

type ES = Either String

subst :: String -> Term -> Term -> Term
subst x val = go
  where
    go :: Term -> Term
    go (TmVar y)       | x == y    = val
                       | otherwise = TmVar y
    go (TmAbs y ty tm) | x == y    = TmAbs y ty tm
                       | otherwise = TmAbs y ty (go tm)
    go (TmApp fun arg)             = TmApp (go fun) (go arg)
    go (TmInt i)                   = TmInt i
    go (TmPlus tm1 tm2)            = TmPlus (go tm1) (go tm2)

evalSubst :: Term -> ES Term
evalSubst (TmVar x)        = pure $ TmVar x
evalSubst (TmAbs x ty tm)  = pure $ TmAbs x ty tm
evalSubst (TmApp fun arg)  = do
  valFun <- evalSubst fun
  valArg <- evalSubst arg
  case valFun of
    TmAbs x _ tm -> evalSubst $ subst x valArg tm
    _            -> Left $ "Applying non-function " ++ show valFun
evalSubst (TmInt i)        = pure $ TmInt i
evalSubst (TmPlus tm1 tm2) = do
  val1 <- evalSubst tm1
  val2 <- evalSubst tm2
  case (val1, val2) of
    (TmInt i1, TmInt i2) -> pure $ TmInt (i1 + i2)
    (TmInt _ , _)        -> Left $ "Invalid operand " ++ show val2
    (_, _)               -> Left $ "Invalid operand " ++ show val1

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["type", filename] -> do
      contents <- readFile filename
      case parseLang contents of
        Nothing -> error "No parse!"
        Just tm -> case runTC (typeCheck tm) [] of
          Left msg -> error msg
          Right ty -> print ty
    ["eval", filename] -> do
      contents <- readFile filename
      case parseLang contents of
        Nothing -> error "No parse!"
        Just tm -> case evalSubst tm of
          Left msg  -> error msg
          Right val -> print val
    _ -> putStrLn "Invalid arguments!"
