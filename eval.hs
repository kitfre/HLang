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
    where one = head lst
          two = head $ tail lst

-- reduce functions for H
reduceExpr :: HVal -> HVal
reduceExpr expr = case expr of
    Atom x        -> Atom x
    HList xs      -> HList $ map reduceExpr xs
    Sum xs        -> HList [Number $ foldl (+) 0 $ map snagNum xs]
    Mul xs        -> HList [Number $ foldl (*) 1 $ map snagNum xs]
    String s      -> String s
    Bool b        -> Bool b
    Init i        -> Init i
    Index xs i    -> reduceExpr ((snagList xs) !! i)
    Elem i xs     -> Bool $ elem i (snagList xs)
    Assign xs i h -> Assign xs i h
    Number i      -> Number i
    If lst        -> if (length lst == 3 && (reduceExpr $ head lst) == Bool True) then (lst !! 1) else (lst !! 2)
    Comp c lst    -> comp c $ map snagNum (snagList lst)
    
runExpr :: String -> String
runExpr str = case readExpr str of
    Just expr -> show (reduceExpr expr)
    Nothing -> "Cannot parse expression"

interp :: IO ()
interp = forever $ do
                     x <- readLn
                     putStrLn (runExpr x)
