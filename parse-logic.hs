-- Parse logical experession like "TRUE AND (TRUE OR FALSE) AND FALSE"

module Main where

import Control.Monad
import System.Environment 
import Text.ParserCombinators.Parsec
import Data.Bool

main :: IO ()
main = getArgs >>= putStrLn . test . (!! 0)

data LogicExpr = BOOL Bool
              | AND LogicExpr LogicExpr 
              | OR LogicExpr LogicExpr 
              | PARENS LogicExpr

logicExprExamples :: [(String, LogicExpr)]
logicExprExamples =
  [("TRUE", BOOL True)
  ,("FALSE", BOOL False)
  ,("TRUE AND FALSE", AND (BOOL True) (BOOL False))
  ,("TRUE AND (TRUE OR FALSE)", AND (BOOL True) (PARENS(OR (BOOL True) (BOOL False))))]

-- Skip whitespaces 
whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

-- lexeme: A style in which every token parser should also consume and ignore any
-- trailing whitespaces.
lexeme :: Parser a -> Parser a
lexeme p = do
  x <- p
  whitespace
  return x

parseBool :: Parser LogicExpr
parseBool = lexeme $ do
  val <- string "TRUE" <|> string "FALSE"
  return $ case val of
    "TRUE" -> BOOL True
    "FALSE" -> BOOL False

parseParens :: Parser LogicExpr
parseParens = do
  lexeme $ char '('
  expr <- lexeme $ (try parseOp <|> parseBool)
  lexeme $ char ')'
  return $ PARENS expr

-- Each operand can be seen as a bool, or an expression inside parentheses
parseBoolOrParens :: Parser LogicExpr
parseBoolOrParens = parseBool <|> parseParens

parseOp :: Parser LogicExpr
parseOp = chainl1 parseBoolOrParens op
  where
    op = do
      val <- lexeme $ (string "AND" <|> string "OR")
      return  $  case val of
        "AND" -> AND
        "OR" -> OR

parseExpr :: Parser LogicExpr
parseExpr = parseParens
            <|> parseOp
            <|> parseBool


parseExpr2 = do
  whitespace
  t <- parseExpr
  return t
  
-- Tell the compiler how to print LogicExpr out   
showExpr :: LogicExpr -> String
showExpr (BOOL b) = show b
showExpr (AND a b) = (showExpr a) ++ " and " ++ (showExpr b)
showExpr (OR a b) = (showExpr a) ++ " or " ++ (showExpr b)
showExpr (PARENS m) = "(" ++ (showExpr m) ++ ")"

instance Show LogicExpr where show = showExpr

-- Evaluate
eval :: LogicExpr -> Bool
eval (BOOL b) = b
eval (AND a b) = (eval a) && (eval b)
eval (OR a b) = (eval a) || (eval b)
eval (PARENS m) = eval m

-- test parser
readExpr :: String -> String
readExpr input = case parse parseExpr "abc" input of
  Left err -> "Invalid expression!"
  Right val -> "Found " ++ show val

test :: String -> String
test input = case parse parseExpr2 "abc" input of
  Left err -> "Invalid expression!"
  Right val -> "Evaluation result: " ++ show (eval val)
