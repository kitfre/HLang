module EvalH where

import Control.Monad
import Text.ParserCombinators.Parsec
import Data.Char
import ParseAtoms 


-- gets numbers out of Numbers
snagNum :: HVal -> Int
snagNum i = case i of
            Number x -> x
            _        -> error "Not a number"

-- gets strings out of Strings
snagStr :: HVal -> String
snagStr s = case s of
            String s -> s
            _        -> error "Not a string"

-- gets list
snagList :: HVal -> [HVal]
snagList lst = case lst of
    HList elems -> elems 
    _           -> error "Not a list"

-- comparison helper
comp :: (Ord a) => String -> [a] -> HVal
comp c lst = case c of
    "<"   -> Bool (one < two)
    ">"   -> Bool (one > two)
    "<="  -> Bool (one <= two)
    ">="  -> Bool (one >= two)
    "=="  -> Bool (one == two)
    "~="  -> Bool (one /= two)
    where one = head lst
          two = head $ tail lst

-- reduce functions for H
reduceExpr :: HVal -> HVal
reduceExpr expr = case expr of
    Atom x                  -> Atom x
    HList xs                -> HList $ map reduceExpr xs
    Sum xs                  -> Number $ foldl (+) 0 $ map (snagNum . reduceExpr) xs
    Mul xs                  -> Number $ foldl (*) 1 $ map (snagNum . reduceExpr) xs
    String s                -> String s
    Bool b                  -> Bool b
    Init i                  -> Init i
    Index xs i              -> reduceExpr ((snagList xs) !! i)
    Elem i xs               -> Bool $ elem i (snagList xs)
    Assign xs i             -> Assign xs i 
    Number i                -> Number i
    If lst                  -> if (length lst == 3) then (if ((reduceExpr $ head lst) == Bool True) then (lst !! 1) else (lst !! 2)) else Error "incorrect list size for if conditional"
    Comp c lst              -> comp c $ map snagNum (snagList lst)
    Function name args body -> Function name args body
    Application name args   -> Application name args
    
runExpr :: String -> HVal
runExpr str = case readExpr str of
    Just expr -> reduceExpr expr
    Nothing -> Error "Cannot parse expression"

prettyPrint :: HVal -> IO ()
prettyPrint val = case val of
    Function name args body -> do
                                putStr (name ++ ": ")
                                putStr ((show args) ++ ", ")
                                putStr (body ++ "\n")
    Atom a                  -> putStrLn a
    Number i                -> putStrLn $ show i
    Bool t                  -> putStrLn $ show t
    HList lst               -> do
                                putStr "[ "
                                printList lst

printList :: [HVal] -> IO ()
printList [] = putStrLn "]"
printList (x:xs) = case x of 
    Atom a    -> do
                   putStr a
                   putStr " "
                   printList xs
    Number i  -> do
                   putStr $ show i
                   putStr " "
                   printList xs
    Bool t    -> do
                   putStr $ show t
                   putStr " "
                   printList xs
    HList lst -> do
                   putStr "[ "
                   printList lst

interp :: IO ()
interp = forever $ do
                     x <- readLn
                     prettyPrint $ runExpr x
