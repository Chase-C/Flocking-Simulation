{-# LANGUAGE DeriveGeneric #-}

module Boid where

--------------------------------------------------------------------------------

import Data.List
import Control.Monad (forM)
--import Control.DeepSeq
--import Control.DeepSeq.Generics (genericRnf)
--import GHC.Generics

import Linear
import Utils

--------------------------------------------------------------------------------

data Boid = Boid
              { bPos  :: V3 Float
              , bVel  :: V3 Float
              , bTar  :: V3 Float
              , bRad  :: !Float
              , bPred :: !Bool
              } deriving (Show, Eq)

--instance NFData Boid where
--    rnf = genericRnf

makeBoids :: (Int, Int, Int) -> (Int, Int, Int) -> Int -> IO [Boid]
makeBoids (lx, ly, lz) (hx, hy, hz) n = forM [1..n] (\_ -> do
        x <- getRandom (rtf lx) (rtf hx) :: IO Float
        y <- getRandom (rtf ly) (rtf hy) :: IO Float
        z <- getRandom (rtf lz) (rtf hz) :: IO Float
        return $ Boid
            { bPos  = V3 x y z
            , bVel  = (V3 x y z) * 0.001
            , bTar  = zero
            , bRad  = 2.5
            , bPred = False
            })
    where rtf = realToFrac

updateBoid :: Boid -> [Boid] -> Boid
updateBoid (Boid pos vel tar rad pred) neighbors =
    let velUpdate = vClamp (foldl (updateVelocity pos) zero neighbors) 0.01
        --tarUpdate = vClamp (updateTarget pos tar) 0.01
        bndUpdate = vClamp (updateBounds pos) 0.015
        newVel    = vScaleTo (vel + velUpdate + bndUpdate) 0.05
    in  Boid
          { bPos  = pos + newVel
          , bVel  = newVel
          , bTar  = zero
          , bRad  = rad
          , bPred = pred
          }

updateBoidRadius :: Boid -> [(Boid, Float)] -> Boid
updateBoidRadius (Boid pos vel tar rad pred) neighbors =
    let velUpdate = vClamp (foldl (updateVelocityRadius pos) zero neighbors) 0.01
        --tarUpdate = vClamp (updateTarget pos tar) 0.01
        bndUpdate = vClamp (updateBounds pos) 0.015
        newVel    = vScaleTo (vel + velUpdate + bndUpdate) 0.05
    in  Boid
          { bPos  = pos + newVel
          , bVel  = newVel
          , bTar  = zero
          , bRad  = rad
          , bPred = pred
          }

updateVelocity :: V3 Float -> V3 Float -> Boid -> V3 Float
updateVelocity pos vel boid
    | pos == bPos boid = vel
    | otherwise        =
        let deltaP = (bPos boid) - pos
            len    = vLen deltaP
        in  (vel + deltaP + ((bVel boid) ^/ (10 * len))) - (deltaP ^* (2.5 / len))

updateVelocityRadius :: V3 Float -> V3 Float -> (Boid, Float) -> V3 Float
updateVelocityRadius pos vel (boid, radius)
    | pos == bPos boid = vel
    | otherwise        =
        let deltaP = (bPos boid) - pos
            len    = radius
        in  (vel + deltaP + ((bVel boid) ^/ (10 * len))) - (deltaP ^* (2.5 / len))

updateTarget :: V3 Float -> V3 Float -> V3 Float
updateTarget pos tar = tar - pos

updateBounds :: V3 Float -> V3 Float
updateBounds pos
    | len > 28  = (zero - pos) ^/ len
    | otherwise = zero
    where len = vLen pos
