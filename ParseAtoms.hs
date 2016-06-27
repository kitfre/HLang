module ParseAtoms where

import Control.Monad
import Text.ParserCombinators.Parsec
import Data.Char



-- init: a = (int)
-- sum: a+
-- prod: a*
-- concat: a ++ b
-- index: a!int
-- assign: a!int <- elem
-- element?: a?elem 

data HVal 
    = Atom String
    | HList [HVal]
    | Number Int
    | String String
    | Bool Bool
    | Init Int
    | Sum [HVal]
    | Mul [HVal]
    | Index HVal Int
    | Elem HVal [HVal]
    | Assign [HVal] Int HVal
    | Concat HVal HVal
    | Comp String HVal
    | If [HVal]
    | Error String
    deriving (Show, Eq)

-- parsing functions

parseAtom :: Parser HVal
parseAtom = do
                first <- letter
                rest <- many $ letter <|> digit
                let atom = first : rest
                return $ case atom of
                    "true"  -> Bool True
                    "false" -> Bool False
                    _       -> Atom atom

parseNumber :: Parser HVal
parseNumber = liftM (Number . read) $ many1 digit

parseInit :: Parser HVal
parseInit = do
                char '('
                x <- digit
                char ')'
                return $ Init $ digitToInt x

parseList :: Parser HVal
parseList = liftM HList $ sepBy parseExpr spaces

-- parse operators
--parseOps :: Parser HVal
--parseOps = parseSum <|> parseMul

parseSum :: Parser HVal
parseSum = liftM Sum $ sepBy parseExpr spaces 

parseMul :: Parser HVal
parseMul = liftM Mul $ sepBy parseExpr spaces

parseIf :: Parser HVal
parseIf = liftM If $ sepBy parseExpr spaces

parseString :: Parser HVal
parseString = do
                char  '"'
                x <- many (noneOf "\"")
                char '"'
                return $ String x

comps = string "<" <|> string ">" <|> string "=" <|> string "~=" <|> string ">=" <|> string "<="

parseExpr :: Parser HVal
parseExpr = parseNumber
         <|> parseInit
         <|> do string "if["
                x <- parseIf
                char ']'
                return x
         <|> parseAtom
         <|> parseString
         <|> do char '['
                x <- parseList
                char ']'
                return x
         <|> do c <- comps
                char '['
                x <- parseList
                char ']'
                return (Comp c x)
         <|> do char '+'
                try (do 
                     char '+'
                     char '['
                     x <- parseList
                     char ']'
                     char '['
                     y <- parseList
                     char ']'
                     return (Concat x y))
                 <|> (do  
                     char '['
                     x <- parseSum
                     char ']'
                     return x)
         <|> do char '*'
                char '['
                x <- parseMul
                char ']'
                return x
         <|> do char '!'
                i <- digit
                char '['
                x <- parseList
                char ']'
                return (Index x (digitToInt i))

readExpr :: String -> Maybe HVal
readExpr input = case parse parseExpr "hvals" input of
    Left err -> Nothing
    Right val -> Just val
