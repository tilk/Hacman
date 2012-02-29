{-# LANGUAGE TupleSections, DoRec, NoMonomorphismRestriction #-}
module Main where

import Level
import LevelPath
import LevelDraw
import qualified Control.DysFRP as R
import Control.DysFRP.Cairo
import Control.DysFRP.Craftwerk
import Graphics.UI.Gtk 
import Control.Arrow
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.State
import Graphics.Craftwerk.Core
import qualified Graphics.Rendering.Cairo as C
import Data.Colour.Names
import Data.Maybe
import Data.NumInstances
import qualified Data.Map as Map

nthMod l k = l !! abs (k `mod` length l)
nthModMaybe [] k = Nothing
nthModMaybe l k = Just $ nthMod l k

rendertext x y r s = do
    C.setSourceRGBA 1 1 1 1
    C.translate x y
    C.rotate $ r / 180 * pi
    C.setFontSize 15
    C.showText s

fround = fromIntegral . round

taxinorm (x1, y1) = abs x1 + abs y1

solidStyle col = style newStyle { lineWidth = Just 0, fill = Just True, fillColor = Just col }
pacmanMouthAngle = 80
pacmanStyle = solidStyle yellow
pacmanF k = scale (0.6, 0.6) $ pacmanStyle $ rotate (k*pacmanMouthAngle/2) $ 
    path [moveTo (0,0), lineTo (1, 0), arc (1, 0) 0 (360 - k*pacmanMouthAngle) 1, lineTo (0, 0)]
pacmanAnim time = fmap (pacmanF . (/2) . (+1) . sin . (*8)) time
pacmanSpeed = 5
superTime = 15
blinkTime = 2

ghostStyle col = solidStyle col
ghostEyeBgStyle = solidStyle white
ghostEyeIrisStyle = solidStyle blue
ghostEyeIrisTranslate = translate . dirCase (-0.5, -0.1) (0.5, -0.1) (0, 0.6) (0, -0.6) (0, 0)
ghostEyeTranslate = translate . dirCase (-0.2, 0) (0.2, 0) (0, 0.2) (0, -0.2) (0, 0)
ghostEye dir = scale (0.3, 0.3) $ composition [scale (1, 1.3) $ ghostEyeBgStyle $ circle (0, 0) 1, 
    ghostEyeIrisTranslate dir $ ghostEyeIrisStyle $ circle (0, 0) 0.5]
ghostFrame col k = scale (0.6, 0.6) $ ghostStyle col $ path [moveTo (1,0), arc (1,0) 0 180 1, 
        lineTo (-1, y1), lineTo (-2/3, y2), lineTo (-1/3, y1), lineTo (0, y2), lineTo (1/3, y1),
        lineTo (2/3, y2), lineTo (1, y1), lineTo (1, 0)]
    where (y1, y2) = (-1 + k * 0.2, -1 - k * 0.2)
ghostEyes dir = scale (0.6, 0.6) $ ghostEyeTranslate dir $ composition 
    [translate (-0.4, 0.2) $ ghostEye dir, translate (0.4, 0.2) $ ghostEye dir]
ghost col dir k = composition [ghostFrame col k, ghostEyes dir]
ghostAnim col dir = ghost col dir . signum . sin . (*5)

ghostColor GBlinky = red
ghostColor GPinky = pink
ghostColor GInky = cyan
ghostColor GClyde = brown

dot DSmall = pacmanStyle $ circle (0,0) 0.12
dot DBig = pacmanStyle $ circle (0,0) 0.3
dots level = composition $ map (\((x, y), d) -> translate (fromIntegral x, fromIntegral y) $ dot d) $ levelDots level where

backgroundStyle = style newStyle { lineWidth = Just 0, fill = Just True, fillColor = Just black } 
background = backgroundStyle $ path $ lineToPath $ unitRectangle

collidesMap map (x, y) | isJust r = fmap (k,) r
                       | otherwise = Nothing
    where 
    k = (round x, round y)
    r = Map.lookup k map

facesWall :: Level -> Dir -> (Double, Double) -> Bool
facesWall level dir (x, y) = dist <= 1.1 && levelMap level (ix + dx, iy + dy) /= FldEmpty
    where 
    (ix, iy) = (round x, round y)
    (dx, dy) = dirToXY dir
    dist = max (abs $ fromIntegral (ix + dx) - x) (abs $ fromIntegral (iy + dy) - y)

correctTurn level (x,y) cdir fdir = oppositeDir cdir == fdir || (cdir /= fdir && (cdir == DirC || abs (fround x - x) < 0.2 && abs (fround y - y) < 0.2) && not (facesWall level fdir (x,y)))

--traceE s = R.ioMapE (\x -> print s >> return x)

wrapAroundX level x | round x < 0 = wrapAroundX level (x + fromIntegral (fst $ levelDimensions level))
                    | round x >= fromIntegral (fst $ levelDimensions level) = wrapAroundX level (x - fromIntegral (fst $ levelDimensions level))
                    | otherwise = x

wrapAroundY level x | round x < 0 = wrapAroundY level (x + fromIntegral (snd $ levelDimensions level))
                    | round x >= fromIntegral (snd $ levelDimensions level) = wrapAroundY level (x - fromIntegral (snd $ levelDimensions level))
                    | otherwise = x

fixGhostLevel level ghtype (pacPos, pacDir) 
    | fromEnum ghtype < 2 = level
    | fromEnum ghtype == 2 = level { levelMap = \p -> if levelWrap level (pacPos + dirToXY pacDir) == p then FldWall else levelMap level p }
    | otherwise = level { levelMap = \p -> if levelWrap level (pacPos - dirToXY pacDir) == p then FldWall else levelMap level p }

game1 difficulty level tick time keys = do 
    pacFDr <- R.stepB DirC dirFEvt
    (pacPos, pacDir) <- sprite level (levelPacPos level) (R.constB pacmanSpeed) pacFDr
    let pacRot = fmap (dirCase 180 0 90 270 0) pacDir
    rec
        let dotEaten = fmap fromJust $ R.whenCondE (liftA2 collidesMap dotsB pacPos) isJust tick
        dotsB <- R.accumB dotmap (fmap (Map.delete . fst) $ dotEaten)
    superPacX <- R.stepB (-superTime) (R.snapshotE time $ R.filterE ((== DBig) . snd) dotEaten)
    let superPac = fmap (\x y -> superTime - x + y) time <*> superPacX
    switchSuper <- fmap (fmap (>0)) $ R.condChangeE (\x y -> x > y || x < 0 && y >= 0) superTime superPac tick
    (ghosts, kills) <- fmap unzip $ mapM (ghost pacPos pacDir superPac switchSuper) $ levelGhosts level
    points <- R.accumB 0 (fmap (const (+1)) dotEaten)
    lvlBg <- prerenderBG (400,400) (render 1 1 $ composition [background, scale (1/ldimF,1/ldimF) $ translate (0.5, 0.5) $ composition [levelFigure level, dots level]]) (fmap (\((x,y),_) -> render ldimF ldimF $ solidStyle black $ path $ lineToPath $ rectangle (fromIntegral x, fromIntegral y) (1,1)) dotEaten)
    let pacman = spriteMove pacPos pacRot $ pacmanAnim time
    let display = concatContextB [lvlBg, renderB 1 1 $ fmap (scale (1/ldimF, 1/ldimF)) $ compositionB (ghosts ++ [pacman]), fmap (mkContext . rendertext 380 20 90 . show) points]
    return display
    where
    respawnPath = levelPath (levelForGhosts level) (levelRespawn level)
    dotmap = Map.fromList $ levelDots level
    ldim = uncurry max $ levelDimensions level
    ldimF = fromIntegral ldim
    dirFEvt = R.concatE [DirL <$ keyLeftE keys, DirR <$ keyRightE keys, DirU <$ keyUpE keys, DirD <$ keyDownE keys]
    blink freq = fmap (even . round . (*freq)) 
    spriteMove spritePos spriteRot = fmap (translate (0.5,0.5)) . translateB spritePos . rotateB spriteRot
    collide pos1 pos2 = R.whenE (fmap ((<0.3) . taxinorm) $ abs $ pos1 - pos2) tick
    fromToPoint d level x toPath = maybe DirC id . (`nthModMaybe` x) . d level toPath . (round *** round)
    toPoint = fromToPoint forwardDirs
    fromPoint = fromToPoint awayDirs
    ghost pacPos pacDir superPac switchSuper (initPos, ghtype) = do
        let roundPacPos = fmap (round *** round) pacPos
        let curPacPath = levelPath <$> fmap (fixGhostLevel (levelForGhosts level) ghtype) (liftA2 (,) roundPacPos pacDir) <*> roundPacPos
        let curPacPathDirect = levelPath <$> R.constB (levelForGhosts level) <*> roundPacPos
        pacMoved <- R.changeE (-1,-1) roundPacPos tick
        rec
            let respawnCollide = collide ghostPos (R.constB $ (fromIntegral *** fromIntegral) $ levelRespawn level)
            ghostAfraid <- R.stepB False (switchSuper `R.appendE` R.constE False respawnCollide)
            pacPath <- R.stepB (emptyPath level) (R.snapshotE (R.ifB ghostAfraid curPacPathDirect curPacPath) pacMoved)
            (ghostPos, ghostRot) <- sprite (levelForGhosts level) initPos (R.ifB ghostAfraid (R.constB 5) (R.constB 3)) (R.ifB ghostEaten toRespawn (R.ifB ghostAfraid fromPac toPac)) 
            let toRespawn = toPoint level (fromEnum ghtype) respawnPath <$> ghostPos 
            let toPac = toPoint level (fromEnum ghtype) <$> pacPath <*> ghostPos
            let fromPac = fromPoint level (fromEnum ghtype) <$> pacPath <*> ghostPos
            let ghostCollide = R.constE ghtype $ collide pacPos ghostPos
            ghostEaten <- R.stepB False $ R.concatE [R.constE True $ R.whenE ghostAfraid ghostCollide, R.constE False $ respawnCollide]
        let ghostImg = spriteMove ghostPos (R.constB 0) $ R.ifB ghostEaten (fmap ghostEyes ghostRot) $ R.ifB (liftA2 (&&) ghostAfraid (liftA2 (||) (fmap (>blinkTime) superPac) (blink 5 superPac))) (ghostAnim blue <$> ghostRot <*> time) (ghostAnim (ghostColor ghtype) <$> ghostRot <*> time)
        return $ (ghostImg, R.whenE (fmap not ghostAfraid) ghostCollide)
    sprite level initPosI velB fdrB = do
        let initPos = (0.5,0) + (fromIntegral *** fromIntegral) initPosI
        rec
            let dirEvt = R.filterWhenE (correctTurn level <$> sPosF <*> sDir) $ R.snapshotE fdrB tick
            sDir <- R.stepB DirC dirEvt
            vel <- velB
            let sSpd = R.ifB (liftA2 (facesWall level) sDir sPosF) (R.constB (0,0)) (fmap (* (vel,vel)) $ fmap dirToXY sDir)
            sPosX <- R.dswitchB (R.constB $ fst initPos) ((R.constE (R.constB . fromIntegral . round) $ R.filterE dirIsV dirEvt) `R.appendE` (fmap (\x -> \v -> fmap (wrapAroundX level . (+v)) x) $ R.genToE (const $ R.trapIntegralB tick time 0 (fmap fst sSpd)) $ R.filterE dirIsH dirEvt)) 
            sPosY <- R.dswitchB (R.constB $ snd initPos) ((R.constE (R.constB . fromIntegral . round) $ R.filterE dirIsH dirEvt) `R.appendE` (fmap (\x -> \v -> fmap (wrapAroundY level . (+v)) x) $ R.genToE (const $ R.trapIntegralB tick time 0 (fmap snd sSpd)) $ R.filterE dirIsV dirEvt)) 
            let sPos = liftA2 (,) sPosX sPosY
            sPosF <- R.feedbackB initPos sPos
        return (sPos, sDir)

game level tick keys = do
    time <- R.elapsedTimeNumB :: R.BehaviorGen Double
    game1 1 level tick time keys

animWindow anim = do
    window <- windowNew
    drawingarea <- drawingAreaNew
    (_, keys) <- reactiveOn window keyPressEvent
    tick <- addTick 25
    on drawingarea realize $ liftIO $ do
        addDrawingAreaRefresherG 50 drawingarea (anim tick keys)
        return ()
    containerAdd window drawingarea
    widgetSetSizeRequest drawingarea 400 400
    on window configureEvent $ do
        (x, y) <- eventSize
        liftIO $ widgetSizeAllocate drawingarea (Rectangle 0 0 x y)
        return True
    return window

main = do
    x <- initGUI
    level <- loadLevel "level1.txt"
    window <- animWindow (game level)
    widgetShowAll window
    on window deleteEvent $ liftIO $ mainQuit >> return True
    mainGUI

