{-
  A basic calculator for arithmetic expressions
  Based on the example in Chapter 8 of "Programming in Haskell"
  by Graham Hutton.

  Pedro Vasconcelos, 2025
-}
module Main where

import Data.Char
import Parsing

--
-- To store variables; simple list of pairs
--
type Env = [(String, Integer)]

--
-- a data type for expressions
-- made up from integer numbers, + and *
--
data Expr
  = Num Integer
  | Add Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Sub Expr Expr
  | Mod Expr Expr
  | Var String
  deriving (Show)

-- a command is either an assignment or just an expression
data Command
  = Assign String Expr -- example: "x" and "1+2"
  | Eval Expr -- example: "1+2" or "x+1"
  deriving (Show)

-- a recursive evaluator for expressions
--
eval :: Expr -> Env -> Integer
eval (Num n) env = n
eval (Add e1 e2) env = (eval e1 env) + (eval e2 env)
eval (Mul e1 e2) env = (eval e1 env) * (eval e2 env)
eval (Div e1 e2) env = (eval e1 env) `div` (eval e2 env)
eval (Sub e1 e2) env = (eval e1 env) - (eval e2 env)
eval (Mod e1 e2) env = (eval e1 env) `mod` (eval e2 env)
eval (Var v) env = find v env

execute :: Env -> String -> (String, Env)
execute env txt =
  case parse command txt of
    -- Case 1: It's an assignment
    [(Assign var e, "")] ->
      let val = eval e env
          newEnv = update var val env
       in (show val, newEnv) -- return val and the New env
      -- Case 2: It's just an expression
    [(Eval e, "")] ->
      let val = eval e env
       in (show val, env) -- return val and the Old env
    _ -> ("Parse Error", env)

-- | a parser for expressions
-- Grammar rules:
--
-- expr ::= term exprCont (Lowest precedence: addition)
-- exprCont ::= '+' term exprCont | epsilon

-- term ::= factor termCont (Medium precedence: multiplication)
-- termCont ::= '*' factor termCont | epsilon

-- factor ::= natural | '(' expr ')' (Highest precedence: numbers/parens)

expr :: Parser Expr
expr = do
  t <- term
  exprCont t

exprCont :: Expr -> Parser Expr
exprCont acc =
  ( do
      char '+'
      t <- term
      exprCont (Add acc t)
  )
    <|> ( do
            char '-'
            t <- term
            exprCont (Sub acc t)
        )
    <|> return acc

term :: Parser Expr
term = do
  f <- factor
  termCont f

termCont :: Expr -> Parser Expr
termCont acc =
  ( do
      char '*'
      f <- factor
      termCont (Mul acc f)
  )
    <|> ( do
            char '/'
            f <- factor
            termCont (Div acc f)
        )
    <|> ( do
            char '%'
            f <- factor
            termCont (Mod acc f)
        )
    <|> return acc

variable :: Parser String
variable = many1 (satisfy isLetter)

factor :: Parser Expr
factor =
  ( do
      n <- natural
      return (Num n)
  )
    <|> ( do
            v <- variable
            return (Var v)
        )
    <|> ( do
            char '('
            e <- expr
            char ')'
            return e
        )

natural :: Parser Integer
natural = do
  xs <- many1 (satisfy isDigit)
  return (read xs)

-- looks up for a variable; crashes if not found
find :: String -> Env -> Integer
find v env = case lookup v env of
  Just n -> n
  Nothing -> error ("Variable" ++ v ++ " not found")

-- update or add a variable to the environment
update :: String -> Integer -> Env -> Env
update var val env = (var, val) : filter ((/= var) . fst) env

command :: Parser Command
command =
  ( do
      v <- variable
      char '='
      e <- expr
      return (Assign v e)
  )
    <|> ( do
            e <- expr
            return (Eval e)
        )

----------------------------------------------------------------

main :: IO ()
main =
  do
    txt <- getContents
    calculator [] (lines txt)

-- | read-eval-print loop
calculator :: Env -> [String] -> IO ()
calculator _ [] = return ()
calculator env (l : ls) = do
  let (output, newEnv) = execute env l
  putStrLn output
  calculator newEnv ls

-- | evaluate a single expression
-- evaluate :: String -> String
-- evaluate txt =
-- case parse expr txt of
-- [(tree, "")] -> show (eval tree)
-- _ -> "parse error; try again"
