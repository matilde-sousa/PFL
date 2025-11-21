{-
  A basic calculator for arithmetic expressions
  Based on the example in Chapter 8 of "Programming in Haskell"
  by Graham Hutton.

  Pedro Vasconcelos, 2025
-}
module Main where

import Parsing
import Data.Char

type Env = [( String , Integer )]


--
-- a data type for expressions
-- made up from integer numbers, + and *
--
data Expr = Num Integer
          | Add Expr Expr
          | Mul Expr Expr
          | Sub Expr Expr
          | Div Expr Expr
          | Mod Expr Expr
          | Var String

          deriving Show

data Command = Assign String Expr | Eval Expr
              deriving Show

-- a recursive evaluator for expressions
--
eval :: Expr -> Env ->Integer
eval (Num n) env = n 
eval (Add e1 e2) env = eval e1 env + eval e2 env
eval (Mul e1 e2) env  = eval e1 env * eval e2 env
eval (Sub e1 e2) env = eval e1 env - eval e2 env
eval (Div e1 e2) env = eval e1 env `div` eval e2 env
eval (Mod e1 e2) env = eval e1 env `mod` eval e2 env
eval (String v) env = find v env

variable :: Parser String
variable = many1 (satisfy isLetter)

command :: Parser Command
command = (do v <- variable 
              char '='
              e <- expr
              return (Assign v e))
          <|> (do e <- expr
                  return (Eval e)) 

find :: String -> Env -> Integer
find v env = case lookup v of 
              Just n -> n
              Nothing -> error "Var Not Found"

update :: String -> Integer -> Env -> Env
update var val env = (var:val) : env


-- | a parser for expressions
-- Grammar rules:
--
-- expr ::= term exprCont
-- exprCont ::= '+' term exprCont | epsilon

-- term ::= factor termCont
-- termCont ::= '*' factor termCont | epsilon

-- factor ::= natural | '(' expr ')'

expr :: Parser Expr
expr = do t <- term
          exprCont t

exprCont :: Expr -> Parser Expr
exprCont acc = do char '+'
                  t <- term
                  exprCont (Add acc t)
               <|> do char '-'
                      t <- term
                      exprCont (Sub acc t)
               <|> return acc
              
term :: Parser Expr
term = do f <- factor
          termCont f

termCont :: Expr -> Parser Expr
termCont acc =  do char '*'
                   f <- factor  
                   termCont (Mul acc f)
                 <|> do char '/'
                        f <- factor  
                        termCont (Div acc f)
                 <|> do char '%'
                        f <- factor  
                        termCont (Mod acc f)
                 <|> return acc

factor :: Parser Expr
factor = do n <- natural
            return (Num n)
          <|>
          do char '('
             e <- expr
             char ')'
             return e
          <|>
          do v <- variable
              return (Var v)
             

natural :: Parser Integer
natural = do xs <- many1 (satisfy isDigit)
             return (read xs)

----------------------------------------------------------------             
  
main :: IO ()
main
  = do txt <- getContents
       calculator (lines txt) []

-- | read-eval-print loop
calculator :: [String] -> Env -> IO ()
calculator [] _ = return ()
calculator (l:ls) env = do 
                      let (output, newEnv) = execute env l
                      putStrLn (output)
                      calculator newEnv ls  

-- | evaluate a single expression
execute :: Env -> String -> ( String , Env )
execute env txt
  = case parse command txt of
      [ (Assign var e, "") ] -> 
        let val = eval e env
            newEnv = update var val env
        in show (val, newEnv)
      [ (Eval e, "") ] -> 
        let val = eval e env
        in show (val, env)
      _ -> error "parse error; try again"  
