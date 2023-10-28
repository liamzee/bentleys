{- cabal:
ghc-options: -O1
default-language: GHC2021
build-depends: base, transformers, random, vector, time, deepseq
-}
{-# LANGUAGE BlockArguments #-}

module Main where

-- base imports.
import Data.Foldable (for_)
import Control.Monad (unless, when, replicateM)
import Control.Monad.Trans.Class (lift)
import Data.Bool (bool)
import Control.Exception (evaluate)
import Control.Monad.ST (runST)
import Data.STRef (modifySTRef', newSTRef, readSTRef)
import Control.Arrow ((&&&))

-- deepseq imports.
import Control.DeepSeq (NFData, force)

-- vector imports.
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM

-- random imports
import System.Random (initStdGen, Uniform, uniform, StdGen)

-- transformers imports
import Control.Monad.Trans.State.Strict qualified as St

-- time imports
import Data.Time.Clock (getCurrentTime, diffUTCTime)

bentleyQSState :: Ord a => Vector a -> Vector a
bentleyQSState vec =
    if 1 >= vLength
      then vec
      else runST do
    mutVec <- V.thaw vec

    let qs start end = when (start < end) $ do
            pivotTarget <- flip St.evalStateT start do
                for_ [start + 1.. end] $ \focus -> do
                    pivTarget <- St.get
                    ordered <- (<) <$> (lift $ VM.read mutVec start)
                               <*> (lift $ VM.read mutVec focus)
                    unless ordered do
                        St.modify' (+1)
                        St.get >>= lift . VM.swap mutVec focus
                St.get
            
            VM.swap mutVec start pivotTarget
            
            qs start (pivotTarget - 1)
            qs (pivotTarget + 1) end
                
    qs 0 (vLength - 1)

    V.freeze mutVec
  where
    vLength = V.length vec

data Bentley = MkBent
    { _start :: !Int
    , _end :: !Int
    , _pivotTarget :: !Int
    }

bentleyQSStateFull :: Ord a => Vector a -> Vector a
bentleyQSStateFull vec =
    if 1 >= vLength
      then vec
      else runST do
    mutVec <- V.thaw vec

    let qs = do
            MkBent start end _ <- St.get
	    when (start < end) $ do
                for_ [start + 1.. end] $ \focus -> do
                    ordered <- (<) <$> (lift $ VM.read mutVec start)
                                   <*> (lift $ VM.read mutVec focus)
                    unless ordered do
		        pivotTarget <- St.gets _pivotTarget
                        St.modify' (\u -> u {_pivotTarget = pivotTarget + 1})
                        lift $ VM.swap mutVec focus (pivotTarget + 1)
            
	        MkBent start end pivotTarget <- St.get
                VM.swap mutVec start pivotTarget
            
	        St.modify' (\u -> u { _end = pivotTarget - 1
				    , _pivotTarget = start
				    }
				    )
                qs

		St.modify' (\u -> u { _start = pivotTarget + 1
		                    , _end = end
		                    , _pivotTarget = pivotTarget + 1
				    }
				    )
                qs
                
    St.evalStateT qs initBent

    V.freeze mutVec
  where
    initBent = MkBent 0 (vLength - 1) 0
    vLength = V.length vec
    
bentleyQSSTRef :: Ord a => Vector a -> Vector a
bentleyQSSTRef vec = let vLength = V.length vec in
    if 1 >= vLength
      then vec
      else runST do
    mutVec <- V.thaw vec

    let qs first last = when (first < last) do
            pivSwapTo <- newSTRef first
            for_ [first+1..last] $ \focus -> do
                ordered <- (<) <$> VM.read mutVec first
                               <*> VM.read mutVec focus
                unless ordered do modifySTRef' pivSwapTo (+1)
                                  VM.swap mutVec focus =<< readSTRef pivSwapTo
                                  
            pivSwapValue <- readSTRef pivSwapTo
            VM.swap mutVec first pivSwapValue
            qs first (pivSwapValue - 1)
            qs (pivSwapValue + 1) last

    qs 0 (vLength - 1)

    V.freeze mutVec


-- | Bentley quicksort function that takes a vector, and if sortable,
-- it copies it into mutable vector within the ST type,
-- and then executes ST operations on it to sort,
-- copying the mutable vector once it's done into an immutable one.
bentleyQSAcc :: Ord a => Vector a -> Vector a
bentleyQSAcc vec = if 1 >= V.length vec           -- Check if sorted already.
    then vec                                      -- by being too short.
    else runST do V.thaw vec >>= go >>= V.freeze  -- Expose to caller.
  where
    go mutVec = qs 0 (V.length vec - 1) 0 1 >> pure mutVec -- Call worker on
                                                           -- mutVec, then
                                                           -- return mutVec.
      where
        qs first last pivSwapTo focus -- Worker is accumulating parameter
                                      -- equivalent of for loop, storing
                                      -- pivot swap target and current focus
                                      -- besides first and last parameters.
                                      --
                                      -- We use this instead of STRef
                                      -- as accumulating parameter
                                      -- outperforms STRef mutables.
                                      
            | first >= last = pure ()  -- If first is equal or greater than
                                       -- last, do nothing and return.
                                       
            | focus > last = do        -- If focus is greater than last,
                                       -- we should swap the pivot
                                       -- with its target, then quicksort
                                       -- again onto the sorted halves.
                VM.swap mutVec first pivSwapTo
                qs first (pivSwapTo - 1) first (first + 1)
                qs (pivSwapTo + 1) last (pivSwapTo + 1) (pivSwapTo + 2)
                
              -- Otherwise, loop into itself. We have a problem insofar as
              -- we have to effectfully read the values of the mutVec,
              -- requiring us to use applicative operators to get a comparison
              -- within the ST type.
              --
              -- We then bind on the result, use bool to do the comparison,
              -- with the first value being false, calling the loop again
              -- with an incremented focus.
              --
              -- If true, however, we swap the smaller element into
              -- the position after the pivot swap destination, then
              -- recurse again with incremented focus and
              -- pivot swap destination.
            | otherwise = (<) <$> VM.read mutVec focus <*> VM.read mutVec first
                >>= bool do qs first last pivSwapTo (focus + 1)
                         do VM.swap mutVec (pivSwapTo + 1) focus
                            qs first last (pivSwapTo + 1) (focus + 1)

bentleyQSAccSplit :: Ord a => Vector a -> Vector a
bentleyQSAccSplit vec = if 1 >= V.length vec              -- Check if sorted already.
    then vec                                              -- by being too short.
    else runST do V.thaw vec >>= go >>= V.freeze          -- Expose to caller.
  where
    go mutVec = qs 0 (V.length vec - 1) >> pure mutVec     -- Call worker on
                                                           -- mutVec, then
                                                           -- return mutVec.
      where
        qs first last = when (first < last) $ qsLoop first (first + 1)
          where
	    qsLoop pivSwapTo focus
	        | focus > last = do VM.swap mutVec first pivSwapTo
		                    qs first (pivSwapTo - 1)
				    qs (pivSwapTo + 1) last
		| otherwise = (>) <$> VM.read mutVec first
		                  <*> VM.read mutVec focus
		    >>= bool do qsLoop pivSwapTo (focus + 1)
		             do VM.swap mutVec (pivSwapTo + 1) focus
			        qsLoop (pivSwapTo + 1) (focus + 1)

makeRandomVector :: Uniform a => Int -> StdGen -> Vector a
makeRandomVector int gen = V.fromList $ go int gen
  where
    go 0 _ = []
    go n gen = uncurry (:) . fmap ( go (n-1) ) $ uniform gen
    
isSortedVec :: Ord a => Vector a -> Bool
isSortedVec vec = go 0
  where
    vLen = V.length vec
    go n
        | n < vLen - 1 = if vec V.! n <= vec V.! (n + 1)
            then go $ n + 1
            else False
        | otherwise = True

seedVector :: IO [Vector Int]
seedVector = replicateM 10000 $ makeRandomVector 1000 <$> initStdGen

timeEval :: NFData a => IO a -> IO a
timeEval action = do
    startTime <- getCurrentTime
    res <- action >>= evaluate . force
    endTime <- getCurrentTime

    mapM_ putStrLn [ ""
                   , "Time it took for this operation:"
                   , show $ diffUTCTime endTime startTime
		   , ""
		   ]
    
    pure res

main :: IO ()
main = do
    seed <- timeEval seedVector

    let test fun = timeEval $ print . and $ isSortedVec . fun <$> seed

    mapM_ (\(str, fun) -> mapM_ putStrLn ["",str,""] >> test fun)
        [ ("StateT", bentleyQSState)
	, ("StateT with full state", bentleyQSStateFull)
        , ("STRef" , bentleyQSSTRef)
	, ("Four-element accumulator", bentleyQSAcc)
	, ("AccParam with inner loop", bentleyQSAccSplit) ]
