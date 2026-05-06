module Main (main) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import Control.Lens hiding (set,(#))
import Control.Monad.State
import Control.Monad
import Data.Map (Map)
import Data.Set (Set)
import Data.Vector (Vector, freeze)
import Data.Vector.Mutable (MVector, PrimState, PrimMonad, unsafeGrow, unsafeExchange, unsafeRead, unsafeWrite)
import System.Environment

import Sort

queryIO :: (Show a) => Oracle IO a
queryIO a b = do
  putStrLn $ "Comparing " ++ show a ++ " and " ++ show b ++ "?"
  putStrLn "  (1) a < b "
  putStrLn "  (2) a > b "
  putStrLn "  (3) neither "
  choice <- readLn :: IO Int
  case choice of
    1 -> return Lt
    2 -> return Gt
    _ -> return Inc

printHasse :: (Ord a, Show a) => (Set a, Set (a,a)) -> IO ()
printHasse (n,s) = do
  let idxes = M.fromList $ zip (S.toList n) [0 :: Int ..]
  putStrLn "digraph G {"
  S.foldr (\a act -> do
    putStrLn $ "  node" ++ show (idxes M.!  a) ++ " [label=\"" ++ show a ++ "\"]"
    act
    ) (return ()) n
  S.foldr (\(a,b) act -> do
    putStrLn $ "  node" ++ show (idxes M.!  b) ++ " -> node" ++ show (idxes M.!  a)
    act
    ) (return ()) s
  putStrLn "}"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [x] -> do
      entries <- lines <$> readFile x
      hd <- calcHasse queryIO (const $ return ()) entries
      printHasse hd
    _ -> putStrLn "Usage: partialSort <filename>"
