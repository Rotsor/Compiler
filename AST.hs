module AST where

data PrimOp = AddP | NegP | MulP | SubP deriving Show
type DefName = String
data Expression = 
  LitE Integer
  | AppE DefName [Expression]
  | CondE Expression Expression Expression
  | PrimE PrimOp [Expression]
  | VarE ParamName deriving Show
type ParamName = String
type Definition = ([ParamName], Expression)
type Program = [(DefName, Definition)]
