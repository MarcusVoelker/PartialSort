module Main (main) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Graphics.UI.Threepenny as UI
import qualified Graphics.UI.Threepenny.SVG as SVG

import Control.Concurrent
import Control.Lens hiding (children,element,set,set',(#))
import Control.Monad
import Data.Bifunctor
import Data.IORef
import Data.List
import Data.Map (Map)
import Data.Ord
import Data.Set (Set)
import Graphics.UI.Threepenny.Core
import Sort

newtype RString = RString String deriving (Eq,Ord)

instance Show RString where
  show (RString s) = s

instance Semigroup RString where
  (<>) (RString a) (RString b) = RString (a <> b)

instance Monoid RString where
  mempty = RString mempty

waitForRef :: IORef (Maybe a) -> IO a
waitForRef ref = do
  res <- readIORef ref
  case res of
    Nothing -> do
      threadDelay 200
      waitForRef ref
    Just a -> do
      atomicWriteIORef ref Nothing
      return a

data VertexKind = P | Q | R deriving (Eq,Ord,Show)

data Vertex a = RealVertex {
  _vertexContent :: a,
  _vertexLayer :: Int,
  _vertexXPos :: Int
  } | DummyVertex {
  _vertexId :: Int,
  _vertexLayer :: Int,
  _vertexXPos :: Int,
  _vertexKind :: VertexKind
  }

instance (Eq a) => (Eq (Vertex a)) where
 (==) (RealVertex a _ _) (RealVertex b _ _) = a == b
 (==) (DummyVertex a _ _ _) (DummyVertex b _ _ _) = a == b
 (==) _ _ = False

instance (Ord a) => (Ord (Vertex a)) where
  compare (RealVertex a _ _) (RealVertex b _ _) = compare a b
  compare (DummyVertex a _ _ _) (DummyVertex b _ _ _) = compare a b
  compare (RealVertex {}) (DummyVertex {}) = LT
  compare (DummyVertex {}) (RealVertex {}) = GT

instance (Show a) => (Show (Vertex a)) where
  show (RealVertex a l x) = show a ++ "@" ++ show (l,x)
  show (DummyVertex _ l x k) = show k ++ "@" ++ show (l,x)

makeFields ''Vertex

rankAllNodes :: (Ord a) => (Set a, Set (a,a)) -> Map a Int
rankAllNodes (nodes,s) = rankAllNodes' (M.fromList (map (,0) (S.toList nodes))) (S.toList nodes)
  where
    rankAllNodes' m [] = m
    rankAllNodes' m (n:ns) = let
      preds = S.toList $ S.map fst $ S.filter ((== n) . snd) s
      succs = S.toList $ S.map snd $ S.filter ((== n) . fst) s
      npred = if null preds then (-1) else maximum $ map (m M.!) preds
      ncur = m M.! n
      in if npred + 1 <= ncur then
        rankAllNodes' m ns
      else
        let
          m' = M.insert n (npred + 1) m
          ns' = filter (not . (`elem` ns)) succs ++ ns
        in rankAllNodes' m' ns'

sparseNorm :: (Monoid a,Ord a) => (Set a, Set (a,a)) -> (Set (Vertex a), Set (Vertex a,Vertex a))
sparseNorm (v,e) = let
  nodeRanks = rankAllNodes (v,e)
  (rvs,lfree) = S.foldr (\vv (s,layerFree) -> let
    rank = nodeRanks M.! vv
    x = M.findWithDefault (0 :: Int) rank layerFree
    lFree' = M.insert rank (x+1) layerFree
    in (S.insert (RealVertex vv rank x) s,lFree')) (S.empty,M.empty) v
  retrieve c = S.findMin $ S.filter (\vv -> vv^.content == c) rvs
  (vs,es) = (\(x,_,_) -> x) $ S.foldr (\(v0,v1) ((vss,ess),free,layerFree) -> let
    l0 = nodeRanks M.! v0
    l1 = nodeRanks M.! v1
    in
      case l1 - l0 of
        1 -> ((vss,S.insert (retrieve v0, retrieve v1) ess),free,layerFree)
        2 -> let
          rank = div (l0+l1) 2
          x = M.findWithDefault (0 :: Int) rank layerFree
          lFree' = M.insert rank (x+1) layerFree
          r = DummyVertex free rank x R
          vss' = S.insert r vss
          ess' = S.insert (retrieve v0, r) $ S.insert (r, retrieve v1) ess
          in ((vss',ess'),free+1,lFree')
        _ -> let 
          rankP = l0+1
          rankQ = l1-1
          x = maximum $ map (\r -> M.findWithDefault (0 :: Int) r layerFree) [rankP..rankQ]
          lFree' = foldr (\r m -> M.insert r (x+1) m) layerFree [rankP..rankQ]
          p = DummyVertex free (l0 + 1) x P
          q = DummyVertex (free+1) (l1 - 1) x Q
          vss' = S.insert q $ S.insert p vss
          ess' = S.insert (retrieve v0, p) $ S.insert (p,q) $ S.insert (q, retrieve v1) ess
          in ((vss',ess'),free+2,lFree')
    ) ((rvs,S.empty),0 :: Int,lfree) e
  in (vs,es)

singleUncross :: [(Int,Int)] -> [Int] -> [(Int,Int)]
singleUncross edges blocked = let
  targets = nub $ map snd edges
  barycenters = map (\t -> (t,(\x -> (fromIntegral (sum x) :: Double) / fromIntegral (length x)) $ map fst $ filter (\e -> snd e == t) edges)) targets
  cands = [0..] \\ blocked
  in zipWith (\i (k,_) -> (k,i)) cands $ sortBy (comparing snd) barycenters


remap :: (Eq a) => [(a,a)] -> a -> a
remap [] y = y
remap ((x,z):xs) y | x == y    = z
                   | otherwise = remap xs y

vRemap :: (Eq a) => (Vertex a -> Bool) -> [(Int,Int)] -> Vertex a -> Vertex a
vRemap c rep v | c v       = v & xPos %~ remap rep
               | otherwise = v

uncross :: (Monoid a,Ord a) => (Set (Vertex a), Set (Vertex a,Vertex a)) -> (Set (Vertex a), Set (Vertex a,Vertex a))
uncross (vs,es) | S.null vs = (vs,es)
                | otherwise = let
  maxLayer = S.findMax $ S.map (^. layer) vs
  in fst $ foldl (\((vs',es'),blocks) l -> let
    edges = map (\(vpre,vpost) -> (vpre ^. xPos, vpost ^. xPos)) $ S.toList $ S.filter (\(vpre,vpost) -> vpre^.layer == l && vpost^.layer == l+1 && vpost ^? kind /= Just Q) es'
    rep = singleUncross edges blocks
    qs = S.map snd $ S.filter (\(vpre,_) -> vpre ^. layer == l+1 && vpre ^? kind == Just P) es'
    vs'' = S.map (vRemap (`S.member` qs) rep) $ S.map (vRemap (\v -> v ^.layer == l + 1 && v ^? kind /= Just Q) rep) vs'
    es'' = S.map (\p -> p & both %~ vRemap (`S.member` qs) rep) $ S.map (\p -> p & both %~ vRemap (\v -> v ^.layer == l + 1 && v ^? kind /= Just Q) rep) es'
    pV = S.toList $ S.map (^. xPos) $ S.filter (\v -> v ^. layer == l+1 && v ^? kind == Just P) vs''
    qV = S.toList $ S.map (^. xPos) $ S.filter (\v -> v ^. layer == l+1 && v ^? kind == Just Q) vs''
    blocks' = (blocks ++ pV) \\ qV
    in ((vs'',es''),blocks')
  ) ((vs,es),[]) [0..maxLayer-1]

node :: (Show a) => Vertex a -> UI Element
node (RealVertex txt _ _) = do
  svgText <- SVG.text
    # set text (show txt)
    # set SVG.x "0"
    # set SVG.y "0"
    # set SVG.text_anchor "middle"
    # set SVG.dominant_baseline "middle"
  svgCircle <- SVG.circle
    # set SVG.cx "0"
    # set SVG.cy "0"
    # set SVG.r "40"
    # set SVG.stroke "black"
    # set SVG.stroke_width "4"
    # set SVG.fill "white"
  SVG.g #+ [pure svgCircle, pure svgText]
node DummyVertex {} = do
  svgCircle <- SVG.circle
    # set SVG.cx "0"
    # set SVG.cy "0"
    # set SVG.r "5"
    # set SVG.stroke "black"
    # set SVG.stroke_width "4"
    # set SVG.fill "white"
  SVG.g #+ [pure svgCircle]

edge :: Vertex a -> Vertex a -> UI Element
edge v0 v1 = do
  let x0 = (v0 ^. xPos) * 100 + 50
  let y0 = (v0 ^. layer) * 100 + 50
  let x1 = (v1 ^. xPos) * 100 + 50
  let y1 = (v1 ^. layer) * 100 + 50
  SVG.polyline
    # set SVG.points (show x0 ++ "," ++ show y0 ++ " " ++ show x1 ++ "," ++ show y1)
    # set SVG.stroke "black"
    # set SVG.stroke_width "4"

svgHasse :: (Monoid a, Ord a, Show a) => Element -> (Set a, Set (a,a)) -> UI ()
svgHasse svg (n,s) = do
  set' children [] svg
  let (vs,es) = uncross $ sparseNorm (n,s)
  set' SVG.width "900" svg
  set' SVG.height "900" svg
  void $
    pure svg
      #+ map (uncurry edge) (S.toList es)
      #+ map (\v -> do
        let x = (v ^. xPos) * 100 + 50
        let y = (v ^. layer) * 100 + 50
        node v
          # set SVG.transform ("translate(" ++ show x ++ " " ++ show y ++ ")")) (S.toList vs)

setupUI :: Window -> UI ()
setupUI w = do
  _ <- pure w # set title "Interactive Partial Sorter"
  b1 <- UI.button # set UI.text "I prefer (1)"
  b2 <- UI.button # set UI.text "I prefer (2)"
  b3 <- UI.button # set UI.text "I prefer neither"
  b4 <- UI.button # set UI.text "Go"
  lb <- UI.label #  set UI.text "Chocolate (1) or Vanilla (2)?"
  input <- UI.textarea
  intext <- stepper "0" $ UI.valueChange input
  _ <- getBody w #+ [row [element lb]]
  _ <- getBody w #+ [row [element b1, element b2, element b3]]
  lb2 <- UI.label # set UI.text "Hier entsteht eine neue Internetpräsenz"
  _ <- getBody w #+ [row [element lb2]]
  choice <- liftIO $ newIORef Nothing
  svg <- SVG.svg
  _ <- getBody w #+ [row [pure svg]]
  _ <- getBody w #+ [row [element input]]
  _ <- getBody w #+ [row [element b4]]
  on UI.click b1 $ \_ -> do
    liftIO $ atomicWriteIORef choice (Just Lt)
  on UI.click b2 $ \_ -> do
    liftIO $ atomicWriteIORef choice (Just Gt)
  on UI.click b3 $ \_ -> do
    liftIO $ atomicWriteIORef choice (Just Inc)
  on UI.click b4 $ \_ -> do
    liftIOLater $ void $ forkIO $ do
      textNow <- currentValue intext
      res <- calcHasse (\a b -> do
        runUI w $ set' UI.text (show a ++ " (1) or " ++ show b ++ " (2)?") lb
        waitForRef choice
        )(\s -> do 
          runUI w $ svgHasse svg s
          runUI w (set' UI.text (show s) lb2)) $ map RString $ lines textNow 
      runUI w $ svgHasse svg res
      runUI w (set' UI.text (show res) lb2)
      runUI w (set' UI.text " " lb)

main :: IO ()
main = startGUI defaultConfig setupUI
