module MatrixGen where

import Data.List
import Data.Maybe
import System.Random

--here we generate some nxn matrices with only 0 and 1 entries

--allRows gives a list of all the possible rows of a nxn matrix

allRows :: Int -> [[Float]]

allRows 1 = [[0], [1]]
allRows n = let nxt = allRows (n-1)
            in map ((:) 0) nxt ++ map ((:) 1) nxt
            
--we don't want 0 rows, so let's filter those            

posRows n = let zs = constV n (0.0 :: Float)
            in filter ((/=) zs) $ allRows n
            
--where constV just produces a list of length n of all a. Probably already exists 
            
constV :: Int -> a0 -> [a0]
constV 0 _ = []
constV n a = a : constV (n-1) a            
            
--finally, all mxn matrices            
            
allMatrices :: Int -> Int -> [[[Float]]]
allMatrices 1 n = map (\a -> [a]) $ posRows n
allMatrices m n = let nxt = allMatrices (m-1) n
                      rws = posRows n
                  in [(r1:rs) | r1 <- rws, rs <- nxt]
                  
--note: don't call this on anything larger than 4x4 unless you've got a 
--supercomputer and some vacation time

--let's find some eigenvalues!

--it might help to do matrix times vector first

mvMult :: [[Float]] -> [Float] -> [Float]

mvMult [] v = []
mvMult m v = foldr (+) 0 (zipWith (*) (head m) v) : mvMult (tail m) v

--now two matrices

mMult :: [[Float]] -> [[Float]] -> [[Float]]
mMult m1 ([]:_) = []
mMult m1 m2 = mvMult m1 (map head m2) : mMult m1 (map tail m2)

--we're going to try the "power iteration" algorithm for finding the largest
--eigenvalue. Thanks to en.wikipedia.org/wiki/Power_iteration

pIter :: [[Float]] -> Float
pIter m = let ph iter nrm b = 
                if iter == 0 then nrm
                else let mProd = mvMult m b
                         newNrm = vNorm mProd
                     in ph (iter-1) newNrm (map (flip (/) newNrm) mProd)
          in ph 10 0 (constV (length (head m)) 1) 
          
--where we need vNorm for the norm of a vector          
          
vNorm :: [Float] -> Float
vNorm ts = sqrt (foldl (+) 0 $ map (flip (^) 2) ts)

--now we're going to make a list of all the matrices and their eigenvalues,
--and sort by eigenvalue

eigMatList :: Int -> [(Float, [[Float]])]
eigMatList n = sortBy eigSort [(pIter m, m) | m <- allMatrices n n]
    where eigSort (e1,_) (e2,_) = compare e1 e2
    
--and grab the closest one to what we want!   
--eigFinder n eig gives a largest eigenvalue, nxn matrix pair with the eigenvalue
--as close as possible to eig  
    
eigFinder :: Int -> Float -> (Float, [[Float]])
eigFinder n eig = 
    let mList = eigMatList n
        maybeInd = findIndex (\(p,_) -> eig < p) mList
        ind = fromMaybe (length mList - 1) maybeInd
    in if ind == 0 then mList!!0
       else let m1 = mList!!ind
                m2 = mList!!(ind-1)
                absDiff = (\(p,_) -> abs (p - eig))
            in if absDiff m1 < absDiff m2 then m1 else m2
            
--now, make a probability matrix out of the 1s and 0s matrix
--the way we want to do it is to have the entries of each row be equal 
--and add to one

toProbMatrix :: [[Float]] -> [[Float]]
toProbMatrix m = map avgRow m
    where avgRow v = let sum = foldl (+) 0 v
                     in map (flip (/) sum) v
                     
--now we'll want a function to map a random double between 0 and 1 to a index of 
--a list of probabilites

mapRDoub :: [Float] -> Float -> Int
mapRDoub ps' d' = let mdh ps d c = let cur = ps!!c 
                                   in if d <= cur then c 
                                      else mdh ps (d-cur) (c+1)
                  in mdh ps' d' 0 

--choose next transformation from a matrix, current state, and random float

pickNext :: [[Float]] -> Int -> Float -> Int
pickNext m cur r = mapRDoub (m!!cur) r

--make a list of transformations according to the probability matrix
--parameters are the matrix, how long you want it, and a random seed

tList :: [[Float]] -> Int -> Int -> [Int]
tList m n seed = 
  let dList = take n (randoms (mkStdGen seed) :: [Float])
      tlh count dl prev = 
          if n == count then []
          else let cur = pickNext m prev (head dl)
               in cur : tlh (count+1) (tail dl) cur
  in tlh 0 dList 0                      
                     
                     
--Peter Lewis, 2012

                













    
