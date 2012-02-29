{-# LANGUAGE RankNTypes #-}
module LevelDraw(levelFigure) where

import Level
import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.Maybe
import Data.STRef
import Data.Array.ST
import Graphics.Craftwerk.Core

wallStyle FldGWall = style newStyle { lineWidth = Just 1, lineColor = Just gray }
wallStyle _ = style newStyle { lineWidth = Just 1, lineColor = Just blue }

toPoint = fromIntegral *** fromIntegral

betweenP (x1,y1) (x2,y2) = ((x1+x2)/2, (y1+y2)/2)
shiftP FldWall  (x1,y1) (x2,y2) (x3,y3) = (x3, y3)
shiftP FldGWall (x1,y1) (x2,y2) (x3,y3) = (x3, y3)
shiftP FldTWall (x1,y1) (x2,y2) (x3,y3) = (x3 + fromIntegral(y1-y2)/7, y3 - fromIntegral(x1-x2)/7)

levelFigure level = runST $ do
    visited <- newArray ((0,0), levelDimensions level) True :: forall s. ST s (STUArray s (Int, Int) Bool)
    paths <- newSTRef []
    let next wt (ox, oy) (x, y) = let
            f (0, 0) [_,_,_, 0,1,_, _,1,_] = Just ( 0,  1)
            f (0, 1) [0,1,_, 1,1,_, _,_,_] = Just (-1,  0)
            f (0, 1) [_,1,_, 0,1,1, 0,0,_] = Just ( 1,  0)
            f (0, 1) [_,1,_, 0,1,_, _,1,_] = Just ( 0,  1)
            f (0, 1) [_,1,_, 0,1,0, 0,0,0] = Just ( 0, -1)
            f _ _ = Nothing
            rot ((x, y), [a,b,c, d,e,f, g,h,i]) = ((y, -x), [c,f,i, b,e,h, a,d,g])
            rrot (x, y) = (-y, x)
            rrots = iterate (. rrot) id
            dp = (x-ox, y-oy)
            env = map (fromEnum . (== wt) . levelMap level) [(x+dx,y+dy) | dy <- [-1..1], dx <- [-1..1]]
            (nx, ny) = fromJust $ msum $ zipWith ($) (map fmap rrots) $ map (uncurry f) $ take 4 $ iterate rot (dp, env)
          in (x+nx, y+ny)
    let analyzeWall wt sp = do
        let nxsp = next wt sp sp
        let an op@(ox,oy) p@(x,y) l | op == sp && p == nxsp && tail l /= [] = return $ reverse l
                                      | otherwise = do
                writeArray visited p False
                let np@(nx,ny) = next wt op p 
                if nx == ox || ny == oy
                  then an p np (lineTo (shiftP wt p np $ toPoint p `betweenP` toPoint np):l)
                  else an p np (curveTo (shiftP wt p np $ toPoint p `betweenP` toPoint np) (shiftP wt op p $ toPoint p) (shiftP wt p np $ toPoint p):l)
        p <- an sp nxsp [moveTo (shiftP wt sp nxsp $ toPoint sp `betweenP` toPoint nxsp)]
        modifySTRef paths (wallStyle wt (path p):)
    forM_ [(x,y) | x <- [0..levelDimX level - 1], y <- [0..levelDimY level - 1]] $ \p -> do
        b <- readArray visited p 
        when b $ case levelMap level p of
            FldEmpty -> return ()
            w -> analyzeWall w p
    composition <$> readSTRef paths

