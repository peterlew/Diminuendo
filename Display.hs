module Display (display,idle,reshape) where
 
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
import GHC.Float 

import System
 
import Square
import TransEx
 
--the size of a new square being drawn
 
squareSize = 0.75 :: GLfloat
 
--the makeTransform function. Pretty simple. Can rotate, translate, flip, or 
--color, though color is currently used as a visual aid to show which 
--transformation is taking place and should not be used in TransformationLists

makeTransform :: Transformation -> GLfloat -> IO ()
              
makeTransform ("Rotate", amt, (x, y, z)) sz = do 
    rotate amt $ Vector3 (0.0 :: GLfloat) 0.0 1.0
makeTransform ("Translate", amt, (x, y, z)) sz = do 
    translate $ Vector3 x y z
makeTransform ("Flip", amt, _) sz = do
    rotate (180*amt) $ Vector3 (0.0 :: GLfloat) 1.0 0.0
    rotate (180*amt) $ Vector3 (1.0 :: GLfloat) 0.0 0.0
    
drawTransList :: TransformationList -> Float -> IO ()      
      
drawTransList [] size = do
    square size
drawTransList (t1:ts) size = do
    makeTransform t1 size
    drawTransList ts size 

--we also need a "gradual" draw function for animation. Only the last square
--in a list is drawn gradually. 

drawTListGrad :: TransformationList -> Float -> Float -> Float 
                    -> Int -> Int -> IO ()
drawTListGrad [] size c lstRot thisT lstT = do

--  draw the last square a bit behind the others
--  this helps it from glitching during animation
    translate $ Vector3 (0.0 :: GLfloat) 0.0 (-0.1) 
    square size                                       
drawTListGrad (t1@(s,a,p@(x,y,z)):ts) size c' lstRot thisT lstT = 
--  start the animation 3/4 of the way through a square's duration
    let c = if c' < 0.75 then 0.0 else 4*(c' - 0.75)
        newT = case s of 
--              Gradual versions of the transformations
                "Rotate" -> if z > 0 && a-lstRot < 0 then 
                               (s,lstRot+(360.0+a-lstRot)*c,p)
                            else if z < 0 && a-lstRot > 0 then
                               (s,lstRot+(a - 360.0 - lstRot)*c,p)
                            else (s,lstRot+(a-lstRot)*c,p)
                "Translate" -> (s,a,(c*x,c*y,c*z))
                "Flip" -> (s,c,p)
    in do
        makeTransform newT size 
        color $ colCombine lstT thisT c   
        drawTListGrad ts size c' lstRot thisT lstT
          
drawList :: [TransformationList] -> [Int] -> Float -> Float -> [Int] -> IO ()
    
drawList (t1:[]) (n1:_) c lstRot (thisT:_) = do
        drawTListGrad t1 (squareSize * sqrt (1.0/(1.0 + c))) c lstRot thisT n1
        undoRotations t1
drawList (t1:ts) (n1:ns) c lstRot (thisT:moreTs) = 
    let colFac = 1.0/(fromIntegral (length ts + 1) + c)
    in do
        color $ reduCol (colMap1 n1) (sqrt colFac)
        drawTransList t1 ((sqrt colFac)*squareSize :: GLfloat) 
        undoRotations t1
        drawList ts ns c (findRot t1) moreTs   
    
display numB counter twist numList durList tmpo = do 
  clear [ColorBuffer,DepthBuffer] 
  loadIdentity

  n <- get numB
  t <- get twist
  c <- get counter
  
--durList is a list of Rationals now, and a value of 1 corresponds to wn, which 
--is 4 beats at tmpo bpm -> 4*60/tmpo seconds. Then we have 30 fps        
   
--if for some reason it's a little off, (could have something to do with rounding,
--or maybe the display function itself takes a little time) 
--just scale the chord durations by 0.98 or 0.99 and that should take care of it
   
  let curDur = 0.98*(3.0*240.0/fromRational tmpo)*durList!!(n-1) 

--this translation here gives a nice little wobble to all the squares    
  translate $ Vector3 (0.02*(cos (t*2*pi/curDur) :: GLfloat)) 
                      (0.02*(sin (t*2*pi/curDur))) 0.0

  if n <= length numList then do -- program not over

      
    --this is interesting ... because of the way drawListGrad works, we are really
    --always drawing the transformation after the one we think we are. So if n == 1,
    --let's say we have drawList [Rotate...] [-1] . It draws the white square and 
    --rotates at the very end. 
     
      drawList (take n (fixRotations (map intMap1 numList)))
                (-1:(take (n-1) numList)) c 0.0 (take n numList)         
                
      if t >= curDur then do
        putStrLn (if n == 0 then "Init" else (show (numList!!(n-1))))
        numB $= n + 1
        twist $= t - curDur
        counter $= 0.0
        swapBuffers
      else do
        twist $= t + 0.1
        counter $= t/curDur
        swapBuffers
        
  else if c <= 10000.0 then do -- finishing condition
          
          if c <= 2.0 then do
              counter $= c + 0.1
          else do
            --spin it
              rotate c $ Vector3 (1.0 :: GLfloat) 1.0 0.0
              counter $= 1.01*c + 10.0          
          
          drawList (fixRotations (map intMap1 numList)) (-1:(take (n-1) numList))
                   1.0 0.0 numList
        

          twist $= t + 0.1
          swapBuffers
        
       else do 
          exitWith ExitSuccess
       
-- idle and reshape callbacks
 
idle = do
  postRedisplay Nothing
  
reshape s@(Size w h) = do 
  viewport $= (Position 0 0, s)  
  
--Peter Lewis, 2012