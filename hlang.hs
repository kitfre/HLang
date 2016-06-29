import ParseAtoms
import EvalH
import Control.Monad
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map as Map

interp :: String -> Context -> Context
interp str cont = case readExpr str of
                    Just h -> case h of
                                Atom a          -> Map.insert a h cont
                                Function n a s  -> Map.insert n h cont
                                _               -> cont

cont :: IORef Context
cont = unsafePerformIO $ newIORef $ Map.empty

main :: IO ()
main = forever $ do
                  str <- readLn
                  modifyIORef cont (interp str)
                  putStrLn $ show $ runExpr str $ unsafePerformIO $ readIORef cont

