{-# LANGUAGE DeriveFunctor #-}
import qualified Data.Map as Map
import Data.Map(Map, (!))
import Control.Applicative
import Control.Monad
import Data.List
import AST
import Parser

type Label = [String]

data InstructionG lbl = 
  JzI lbl
  | JmpI lbl
  | CallI String
  | LabelI lbl
  | SetEax Integer
  | TestEax
  | PushEax
  | PopEbx
  | PopEax
  | AddEaxEbx
  | SubEaxEbx
  | MulEaxEbx
  | ArgToEax Int -- ArgToEax n loads the nth argument into EAX
  | Enter
  | Leave Int
    deriving (Functor, Show)

type Instruction = InstructionG Label

printInstruction :: InstructionG String -> [String]
printInstruction (ArgToEax i) = ["  mov eax, [ebp+"++show((i+2)*4)++"]"]
printInstruction (JzI lbl) = ["  jz " ++ lbl]
printInstruction (JmpI lbl) = ["  jmp " ++ lbl]
printInstruction (CallI lbl) = ["  call " ++ lbl]
printInstruction (LabelI lbl) = [lbl ++ ":"]
printInstruction TestEax = ["  test eax, eax"]
printInstruction AddEaxEbx = ["  add eax, ebx"]
printInstruction SubEaxEbx = ["  sub eax, ebx"]
printInstruction MulEaxEbx = ["  mul eax, ebx"]
printInstruction (SetEax x) = ["  mov eax, " ++ show x]
printInstruction PushEax = ["  push eax"]
printInstruction PopEbx = ["  pop ebx"]
printInstruction PopEax = ["  pop eax"]
printInstruction Enter = ["  push ebp", "  mov ebp, esp"]
printInstruction (Leave nargs) = ["  pop ebp", "  ret " ++ show (nargs * 4)]

printInstructions = concatMap (printInstruction . fmap (intercalate "_"))

prog0 = [("id", (["x"], CondE (VarE "x") (LitE 8) (LitE 9)))]

prog1 = 
  [ ("fact", (["x"],
     CondE
     (VarE "x")
     (PrimE MulP
      [ (VarE "x") 
      , (AppE "fact" [PrimE AddP [VarE "x", LitE (-1)]])
      ]
     )
     (LitE 1))
    )
  , ("fib", (["x"], 
             CondE
             (VarE "x")
             (CondE
              (PrimE SubP [VarE "x", LitE (1)])
              (PrimE AddP [(AppE "fib" [(PrimE AddP [VarE "x", LitE (-1)])]), (AppE "fib" [(PrimE AddP [VarE "x", LitE (-2)])])])
              (LitE 1)
             )
             (LitE 1)
             ))
    , ("sub", (["x", "y"],
               PrimE SubP [VarE "x", VarE "y"]))
  ]

data InterpretationError = 
  NoDef DefName
  | NoParam DefName ParamName
  | ParamListMismatch deriving Show

data Interpretation a =
  Error InterpretationError
  | Success a deriving Show

instance Monad Interpretation where
  Error e >>= f = Error e
  Success x >>= f = f x
  
  return = Success

instance Functor Interpretation where
  fmap f = (pure f <*>)

instance Applicative Interpretation where
  (<*>) = ap
  pure = return

szip a b | length a == length b = Success $ zip a b 
 | otherwise = Error ParamListMismatch

interpret :: Program -> DefName -> [Integer] -> Interpretation Integer
interpret prog name paramValues =
  case lookup name prog of
  Nothing -> Error (NoDef name)
  Just (paramNames, body) -> do
    params <- szip paramNames paramValues
    let
      go (LitE x) = Success x
      go (AppE def params) = interpret prog def =<< mapM go params
      go (CondE c t f) = go c >>= (\c -> if c /= 0 then go t else go f)
      go (PrimE MulP [x,y]) = (*) <$> go x <*> go y
      go (PrimE AddP [x,y]) = (+) <$> go x <*> go y
      go (PrimE SubP [x,y]) = (-) <$> go x <*> go y
      go (PrimE NegP [x]) = negate <$> go x
      go (PrimE _ _) = Error ParamListMismatch
      go (VarE x) = case lookup x params of
        Nothing -> Error (NoParam name x)
        Just v -> Success v
    go body

primCode :: PrimOp -> [Instruction]
primCode AddP = [PopEbx, PopEax, AddEaxEbx]
primCode MulP = [PopEbx, PopEax, MulEaxEbx]
primCode SubP = [PopEbx, PopEax, SubEaxEbx]
primCode NegP = undefined

labelScope scope = fmap ([scope] ++)

compile1 :: Map String [Instruction] -> (DefName, Definition) -> Map String [Instruction]
compile1 m (name, (params, body)) = Map.insert name code m where
  code = [LabelI [name], Enter] ++ map (labelScope name) (go body) ++ [Leave (length params)]
  pushAll params = (\(i,p) -> map (labelScope ("p" ++ show i)) (go p) ++ [PushEax]) =<< zip [1..] params
  go (LitE x) = [SetEax x]
  go (AppE def params) = pushAll params ++ [CallI def]
  go (PrimE prim [x, y]) = pushAll [x, y] ++ primCode prim
  go (PrimE prim _) = error "bad primitive call"
  go (VarE x) = [ArgToEax (head $ [i | (i, p) <- reverse (zip [0..] (reverse params)), p == x])]
  go (CondE c t f) = let g c = map (labelScope "_") (go c) in g c ++ [TestEax, JzI ["L"]] ++ g t ++ [JmpI ["E"], LabelI ["L"]] ++ g f ++ [LabelI ["E"]]

compile :: Program -> (Map String [Instruction])
compile = foldl compile1 Map.empty


params = [40]
func = "fib"

main = do
  prefix <- readFile "prefix.asm"
  prog <- parseFile "fib.co"
  
  writeFile "output.asm" $ prefix ++ (unlines $ 
                                      (map (\x -> "  push " ++ show x) params) ++ ["  call " ++ func] 
                                      ++ [ "  push eax","  push message", "  call printf", "  add esp, 8", "  ret"]
                                      ++ ["message:", "  db 'Result: %d', 13, 0"]
                                      ++ (printInstructions $ compile prog ! func))

