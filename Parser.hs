module Parser where

import AST
import Text.ParserCombinators.ReadP
import Data.Char
import Control.Applicative ((<*), (*>), (<*>), (<$>), Applicative, pure)
import Control.Monad

instance Applicative ReadP where
  (<*>) = ap
  pure = return

w = many (satisfy isSpace)
w1 = many1 (satisfy isSpace)

identifier = many1 (satisfy isAlpha)

definition :: ReadP (DefName, Definition)
definition = mkDef <$> identifier <*> parameterList <* char  '=' <*> expression where
  mkDef :: String -> [String] -> Expression -> (DefName, Definition)
  mkDef name params body = (name, (params, body))
  
  parameterList :: ReadP [String]
  parameterList = many (w1 *> identifier) <* w

application p = AppE <$> (w *> identifier) <*> (many (w1 *> p) <* w)

hacke :: Expression -> Expression
hacke (AppE n []) = VarE n
hacke (AppE n e) = AppE n (map hacke e)
hacke (PrimE n e) = PrimE n (map hacke e)
hacke (CondE c t f) = CondE (hacke c) (hacke t) (hacke f)
hacke (LitE n) = LitE n

hack :: Program -> Program
hack = map hackq
hackq (n, (p, b)) = (n, (p, hacke b))

expression :: ReadP Expression
expression = p_cond where
    p_cond = do
      a <- p_add
      (CondE a <$> (char '?' *> p_cond <* char ':') <*> p_cond) 
        +++ return a
    p_add = do
      a <- p_mul
      (((\b -> PrimE AddP [a, b]) <$> (char '+' *> p_add))
        +++ ((\b -> PrimE SubP [a, b]) <$> (char '-' *> p_add))
        +++ return a)
    p_mul = ((\a b -> PrimE MulP [a, b]) <$> (p_app <* char '*') <*> p_mul) +++ p_app
    p_app = application p_var +++ p_var
    p_var = w *> ((LitE . read) <$> many1 (satisfy isDigit)) +++ p_paren <* w
    p_paren = (char '(' *> p_cond <* char ')')

parser :: ReadP Program
parser = many (definition <* char '\n') <* eof

parseFile fileName = readFile fileName >>= \t -> case readP_to_S parser t of
  ((r, []):_) -> return (hack r)
