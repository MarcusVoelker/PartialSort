module Sort (calcHasse, Oracle, Handler, Comp(..)) where

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

newtype ChainDecomp m a = CD {
  _chainDecompChains :: MVector (PrimState m) (MVector (PrimState m) a)
  }

makeClassy ''ChainDecomp

data Comp = Lt
  | Gt
  | Inc
  deriving (Eq, Show)

nComp :: Comp -> Comp
nComp Lt = Gt
nComp Gt = Lt
nComp Inc = Inc

data SortState m a = SortState {
  _sortStateDecomp :: ChainDecomp m a,
  _sortStateCompMatrix :: Map (a,a) Comp,
  _sortStateOpen :: [a]
  }

makeFields ''SortState

data SortResult a = SortResult {
  _sortResultDecomp :: Vector (Vector a),
  _sortResultCompMatrix :: Map (a,a) Comp
  }

instance (Show a) => (Show (SortResult a)) where
  show (SortResult dec mat) = show dec ++ " " ++ show mat

makeFields ''SortResult

insertAt :: (PrimMonad m) => MVector (PrimState m) a -> Int -> a -> m (MVector (PrimState m) a)
insertAt vec pos val = do
  vec' <- unsafeGrow vec 1
  foldM_ (flip (unsafeExchange vec')) val [pos..MV.length vec]
  return vec'

type Oracle m a = (Monad m) => a -> a -> m Comp
type Handler m a = (Monad m) => (Set a, Set (a,a)) -> m ()

query :: (Monad m, Ord a, HasCompMatrix s (Map (a,a) Comp)) => Oracle m a -> a -> a -> StateT s m Comp
query oracle e0 e1 = do
  mat <- use compMatrix
  if M.member (e0,e1) mat
  then return (mat M.! (e0,e1))
  else do
    val <- lift $ oracle e0 e1
    compMatrix %= M.insert (e0,e1) val
    compMatrix %= M.insert (e1,e0) (nComp val)
    return val

findLt :: (PrimMonad m, Ord a, HasCompMatrix s (Map (a,a) Comp)) => Oracle m a -> MVector (PrimState m) a -> a -> Int -> Int -> StateT s m Int
findLt oracle chain val lb ub
  | lb == ub = do
    cv <- MV.unsafeRead chain lb
    c <- query oracle cv val
    case c of
      Lt -> return $ lb + 1
      _ -> return lb
  | lb + 1 == ub = do
    uv <- MV.unsafeRead chain ub
    u <- query oracle uv val
    case u of
      Lt -> return (ub+1)
      _ -> do
        lv <- MV.unsafeRead chain lb
        l <- query oracle lv val
        case l of
          Lt -> return ub
          _ -> return lb
  | otherwise = do
    let center = div (lb + ub) 2
    cv <- MV.unsafeRead chain center
    c <- query oracle cv val
    case c of
      Lt -> findLt oracle chain val center ub
      Gt -> findLt oracle chain val lb center
      Inc -> findLt oracle chain val lb center

findGt :: (PrimMonad m, Ord a, HasCompMatrix s (Map (a,a) Comp)) => Oracle m a -> MVector (PrimState m) a -> a -> Int -> Int -> StateT s m Int
findGt oracle chain val lb ub
  | lb == ub = do
    cv <- MV.unsafeRead chain lb
    c <- query oracle cv val
    case c of
      Gt -> return lb
      _ -> return $ lb + 1
  | lb + 1 == ub = do
    lv <- MV.unsafeRead chain lb
    l <- query oracle lv val
    case l of
      Gt -> return lb
      _ -> do
        uv <- MV.unsafeRead chain ub
        u <- query oracle uv val
        case u of
          Gt -> return ub
          _ -> return $ ub + 1
  | otherwise = do
    let center = div (lb + ub) 2
    cv <- MV.unsafeRead chain center
    c <- query oracle cv val
    case c of
      Lt -> findGt oracle chain val center ub
      Gt -> findGt oracle chain val lb center
      Inc -> findGt oracle chain val center ub

compareChain :: (PrimMonad m, Ord a, HasCompMatrix s (Map (a,a) Comp)) => Oracle m a -> MVector (PrimState m) a -> a -> StateT s m (Maybe Int)
compareChain oracle chain val = do
  lv <- MV.unsafeRead chain 0
  uv <- MV.unsafeRead chain (MV.length chain - 1)
  lc <- query oracle lv val
  case lc of
    Gt -> do
      MV.forM_ chain $ \c -> do
        compMatrix %= M.insert (c,val) Gt
        compMatrix %= M.insert (val,c) Lt
      return (Just 0)
    Inc -> do
      ub <- findGt oracle chain val 0 (MV.length chain - 1)
      let (incs,gts) = MV.splitAt ub chain
      MV.forM_ incs $ \c -> do
        compMatrix %= M.insert (c,val) Inc
        compMatrix %= M.insert (val,c) Inc
      MV.forM_ gts $ \c -> do
        compMatrix %= M.insert (c,val) Gt
        compMatrix %= M.insert (val,c) Lt
      return Nothing
    Lt -> do
      uc <- query oracle uv val
      case uc of
        Lt -> do
          MV.forM_ chain $ \c -> do
            compMatrix %= M.insert (c,val) Lt
            compMatrix %= M.insert (val,c) Gt
          return (Just (MV.length chain))
        Inc -> do
          lb <- findLt oracle chain val 0 (MV.length chain - 1)
          let (lts,incs) = MV.splitAt lb chain
          MV.forM_ lts $ \c -> do
            compMatrix %= M.insert (c,val) Lt
            compMatrix %= M.insert (val,c) Gt
          MV.forM_ incs $ \c -> do
            compMatrix %= M.insert (c,val) Inc
            compMatrix %= M.insert (val,c) Inc
          return Nothing
        Gt -> do
          lb <- findLt oracle chain val 0 (MV.length chain - 1)
          ub <- findGt oracle chain val 0 (MV.length chain - 1)
          let (h,gts) = MV.splitAt ub chain
          let (lts,incs) = MV.splitAt lb h
          MV.forM_ lts $ \c -> do
            compMatrix %= M.insert (c,val) Lt
            compMatrix %= M.insert (val,c) Gt
          MV.forM_ incs $ \c -> do
            compMatrix %= M.insert (c,val) Inc
            compMatrix %= M.insert (val,c) Inc
          MV.forM_ gts $ \c -> do
            compMatrix %= M.insert (c,val) Gt
            compMatrix %= M.insert (val,c) Lt
          if MV.null incs then
            return (Just lb)
          else
            return Nothing

handleNext :: (PrimMonad m, Ord a) => Oracle m a -> StateT (SortState m a) m ()
handleNext oracle = do
  (CD cd) <- use decomp
  next <- use open
  case next of
    [] -> return ()
    (h:t) -> do
      open .= t
      added <- MV.ifoldM (\added i chain  -> do
        place <- compareChain oracle chain h
        case place of
          Nothing -> return added
          Just loc -> do
            unless added $ do
              vecnew <- insertAt chain loc h
              unsafeWrite cd i vecnew
            return True) False cd
      unless added $ do
        cd' <- unsafeGrow cd 1
        vecnew <- MV.new 1
        unsafeWrite vecnew 0 h
        unsafeWrite cd' (MV.length cd) vecnew
        decomp .= CD cd'

handleAll :: (PrimMonad m, Ord a) => Oracle m a -> Handler m a -> StateT (SortState m a) m ()
handleAll oracle handler = do
  next <- use open
  case next of
    [] -> return ()
    _ -> do
      cur <- use compMatrix
      lift $ handler (S.map fst $ M.keysSet cur,reduction cur)
      handleNext oracle
      handleAll oracle handler

deepFreeze :: (PrimMonad m) => MVector (PrimState m) (MVector (PrimState m) a) -> m (Vector (Vector a))
deepFreeze vec = do
  f1 <- MV.generateM (MV.length vec) $ \i -> do
    svec <- unsafeRead vec i
    freeze svec
  freeze f1

pSort :: (PrimMonad m, Ord a) => Oracle m a -> Handler m a -> [a] -> m (SortResult a)
pSort _ _ [] = return $ SortResult V.empty M.empty
pSort oracle handler (e0:elems) = do
  vec0 <- MV.new 1
  vec00 <- MV.new 1
  unsafeWrite vec00 0 e0
  unsafeWrite vec0 0 vec00
  let state0 = SortState (CD vec0) M.empty elems
  (SortState (CD dec) mat _) <- execStateT (handleAll oracle handler) state0
  decF <- deepFreeze dec
  return $ SortResult decF mat

reduction :: (Ord a) => Map (a,a) Comp -> Set (a,a)
reduction m = let
  res = M.keysSet $ M.filter (== Lt) m
  as = S.map fst $ M.keysSet m
  in
    foldr (\k s ->
      foldr (\i s' ->
        foldr (\j s'' ->
          if S.member (i,k) s'' && S.member (k,j) s'' then
            S.delete (i,j) s''
          else
            s''
          ) s' as
        ) s as
      ) res as

calcHasse :: (PrimMonad m, Ord a) => Oracle m a -> Handler m a -> [a] -> m (Set a, Set (a,a))
calcHasse oracle handler elems = do
  (SortResult _ mat) <- pSort oracle handler elems
  return (S.map fst $ M.keysSet mat,reduction mat)
