module ParseAtoms where

import Control.Monad
import Text.ParserCombinators.Parsec
import Data.Char
import Data.List.Split (splitOn)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map as Map


-- init: a = (int)
-- sum: a+
-- prod: a*
-- concat: a ++ b
-- index: a!int
-- assign: f<-[\x -> f(x)]
-- element?: a?elem 

data HVal 
    = Atom String
    | Function String [String] String
    | Application String [String]
    | HList [HVal]
    | Number Int
    | String String
    | Bool Bool
    | Init Int
    | Sum [HVal]
    | Mul [HVal]
    | Minus [HVal]
    | Index HVal Int
    | Elem HVal HVal
    | Assign String HVal
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

parseSum :: Parser HVal
parseSum = liftM Sum $ sepBy parseExpr spaces 

parseMul :: Parser HVal
parseMul = liftM Mul $ sepBy parseExpr spaces

parseMinus :: Parser HVal
parseMinus = liftM Minus $ sepBy parseExpr spaces

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
         <|> do char '\''
                name <- many1 (noneOf "[")
                char '['
                spaces
                args <- many1 (noneOf "]")
                char ']'
                return (Application name (splitOn " " args))
         <|> try  (do 
                    string "->" 
                    name <- many1 (noneOf "[")
                    char '['
                    spaces
                    args <- (endBy1 (many1 (noneOf " .\\()")) spaces)
                    char '.'
                    spaces
                    body <- many1 (noneOf "]")
                    char ']'
                    return (Function name args (body ++ "]")))
         <|> do string "if["
                x <- parseIf
                char ']'
                return x
         <|> do char '?'
                x <- parseExpr
                char '['
                y <- parseList
                char ']'
                return (Elem x y)
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
         <|> do char '-'
                char '['
                x <- parseMinus
                char ']'
                return x
         <|> do char '!'
                i <- digit
                char '['
                x <- parseList
                char ']'
                return (Index x (digitToInt i))
         <|> parseAtom
         <|> parseString

type Context = Map.Map String HVal

readExpr :: String -> Maybe HVal
readExpr input = case parse parseExpr "hlang" input of
    Left err -> Nothing
    Right val -> Just val

parseFile :: FilePath -> Context
parseFile f = buildContext  Map.empty (lines $ unsafePerformIO $ readFile f)

buildContext :: Context -> [String] -> Context
buildContext m []     = m
buildContext m (x:xs) = let res = case readExpr x of 
                                    Just h -> h
                                    Nothing -> Error "no parse"
                        in
                            case res of
                                Atom a          -> buildContext (Map.insert a res m) xs 
                                Function n a b  -> buildContext (Map.insert n res m) xs
                                _               -> buildContext m xs
