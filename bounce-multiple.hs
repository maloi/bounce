{-# LANGUAGE TemplateHaskell #-}
module Main where


import Control.Applicative ((<$>))
import Control.Monad.State
import Control.Lens hiding (at)
import System.Random (randomRs, newStdGen)

import Graphics.Gloss

data Ball = Ball
    { _ballPos :: (Float, Float)
    , _ballSpeed :: (Float, Float)
    }
makeLenses ''Ball

data World = World
    { _balls :: [Ball]
    }
makeLenses ''World

winPosX      = 0
winPosY      = 0
winWidth     = 1920
winHeight    = 1080
initialSpeed = 1000
fps          = 60
numBalls     = 5000
backColor    = white

initialWorld :: World
initialWorld = World $ replicate numBalls (Ball (0, 0) (0, 0))

drawBall :: Picture
drawBall = circleSolid 5

draw :: World -> Picture
draw w = Pictures [ drawBall `at` b^.ballPos | b <- w^.balls ]
    where
      p `at` (x,y) = translate x y p; infixr 1 `at`

update :: a -> Float -> World -> World
update _ dt = execState $ do
    updateBalls dt
    checkBounds

updateBalls :: Float -> State World ()
updateBalls dt = do
    w <- get
    let w' = w { _balls = (over mapped updateBall (_balls w)) }
    put w'
    where
      updateBall b = ballPos +~ (dt * b^.ballSpeed._1, dt * b^.ballSpeed._2) $ b

checkBounds :: State World ()
checkBounds = do
    w <- get
    let w' = w { _balls = (map collX (_balls w)) }
    let w'' = w' { _balls = (map collY (_balls w')) }
    put w''

    where
      collX b = if (abs (b^.ballPos._1) >= (fromIntegral winWidth) / 2)
                then Ball (b^.ballPos) (negate (b^.ballSpeed._1), (b^.ballSpeed._2))
                else Ball (b^.ballPos) (b^.ballSpeed)
      collY b = if (abs (b^.ballPos._2) >= (fromIntegral winHeight) / 2)
                then Ball (b^.ballPos) ((b^.ballSpeed._1), negate (b^.ballSpeed._2))
                else Ball (b^.ballPos) (b^.ballSpeed)


main :: IO ()
main = do
    bs <- mapM initBall (initialWorld^.balls)
    let world = World bs
    simulate displayMode backColor fps world draw update

    where
      displayMode = InWindow "Pong" (winWidth, winHeight) (winPosX, winPosY)
      initBall b = do
        speeds <- randomVector initialSpeed 0.2
        locations <- randomVector ((fromIntegral winHeight) / 2) (fromIntegral (-winHeight))
        return $ ballPos .~ locations $ ballSpeed .~ speeds $ b


randomVector :: Float -> Float -> IO (Float, Float)
randomVector b f = do
    rs <- randomRs (-b, b) <$> newStdGen
    return . toPair . take 2 $ filter ((> f) . abs) rs

    where
      toPair [] = undefined
      toPair [_] = undefined
      toPair (x:y:_) = (x,y)
