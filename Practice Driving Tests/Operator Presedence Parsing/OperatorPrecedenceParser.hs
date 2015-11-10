import Data.Char
import Data.Maybe

type Operator = Char

data Expr = Num Int | Var String | Op Operator | App Expr Expr Expr
            deriving (Eq,Ord,Show)

type Token = Expr

data Associativity = L | N | R
                     deriving (Eq,Ord,Show)

ops :: [Operator]
ops = "+-*/^()$"

type Precedence = Int

opTable :: [(Operator, (Precedence, Associativity))]
opTable
  = [ ('$',(0,N))
    , ('(',(1,N))
    , ('+',(6,L))
    , ('-',(6,L))
    , ('*',(7,L))
    , ('/',(7,L))
    , ('^',(8,R))
    , (')',(1,N))
    ]

type ExprStack = [Expr]

type OpStack = [Operator]

showExpr :: Expr -> String
showExpr (Num n) 
  = show n 
showExpr (Var s) 
  = s
showExpr (Op c)  
  = [c]
showExpr (App op e e') 
  = "(" ++ showExpr e ++ showExpr op ++ showExpr e' ++ ")"

--------------------------------------------

--
-- Assume throughout that all function arguments are valid, for example:
--   All input expressions are well-formed
--   All Operators (Chars) are members of 'ops' above
--   The stacks passed to buildOpApp and parse constitute valid `state'
--   with respect to the Shunting Yard algorithm
--


-------------------------------------------------------------------
precedence :: Operator -> Precedence
precedence op
  = fst $ fromJust $ lookup op opTable
 
associativity :: Operator -> Associativity
associativity op
  = snd $ fromJust $ lookup op opTable

higherPrecedence :: Operator -> Operator -> Bool
higherPrecedence op1 op2
  = precedence op1 > precedence op2

eqPrecedence :: Operator -> Operator -> Bool
eqPrecedence op1 op2
  = precedence op1 == precedence op2

isRightAssociative :: Operator -> Bool
isRightAssociative op
  = associativity op == R

supersedes :: Operator -> Operator -> Bool
supersedes op1 op2
  = higherPrecedence op1 op2 ||
    (eqPrecedence op1 op2 && isRightAssociative op1)

stringToInt :: String -> Int
stringToInt str
  = foldl1 (\a b -> 10*a + b) (digitList str)
  where
    digitList = map (\c -> ord c - ord '0')

buildExpr :: String -> Expr
buildExpr s
  = parse (tokenise s) ([], ['$'])

tokenise :: String -> [Token]
tokenise str@(c:cs)
  | isSpace c  = tokenise cs
  | elem c ops = Op c : tokenise cs
  | otherwise  = token : tokenise strRem
  where
    (token, strRem) = extractToken c str
    extractToken start str
      | isDigit start = (Num (stringToInt tokenStr), strRem)
      | otherwise     = (Var tokenStr, strRem)
      where
        (tokenStr, strRem) = break (not . isAlphaNum) str
tokenise []
  = []

buildOpApp :: (ExprStack, OpStack) -> (ExprStack, OpStack)
buildOpApp ((r:l:exps), (op:ops))
  = ((app:exps), ops)
  where
    app = App (Op op) l r

parse :: [Token] -> (ExprStack, OpStack) -> Expr
parse [] ([final], _)
  = final
parse [] stacks
  = parse [] (buildOpApp stacks)
parse (t:ts) (exps, allOps@(op:_))
  = case t of
      Op to -> parse ts (processOperator to)
      _     -> parse ts (t:exps, allOps)
      where
        processOperator to
          | supersedes to op = (exps, to:allOps)
          | otherwise        = (newExps, to:newOps)
            where
              (newExps, newOps) = buildOpApp (exps, allOps)
