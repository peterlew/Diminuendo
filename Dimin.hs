import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
import Control.Concurrent
 
import System.Random 
 
import Euterpea
 
import Display
import Music
import TransEx
import MatrixGen
 
--these two functions are just for printing the transformation matrix 
 
rowToString :: [Float] -> String
rowToString [] = ""
rowToString (f1:fs) = show f1 ++ " " ++ rowToString fs
 
printMatrix :: [[Float]] -> IO ()
printMatrix [] = return ()
printMatrix (r1:rs) = do 
    putStrLn (rowToString r1)
    printMatrix rs 
 
main = do
  putStrLn "Enter your desired eigenvalue"
  eigen <- getLine 
  let (fEig, eMatr) = eigFinder 4 (read eigen)
      pMatr = toProbMatrix eMatr
  putStrLn ("Eigenvalue found: " ++ show fEig)
  putStrLn "With transition matrix:"
  printMatrix eMatr
  putStrLn "How many iterations do you want?"
  iterCount <- getLine
  putStrLn "Ok. And at what tempo?"
  tmpo' <- getLine
  putStrLn "Calibrate your MIDI instrument now"
--  play some notes to allow the musician to get the right patch
  play $ instrument RhodesPiano (timesM 16 (c 4 sn :=: e 4 sn :=: g 4 sn))
  putStrLn "Press Enter when ready"
  getLine
  let numList = tList pMatr (read iterCount) (truncate (100*fEig))
      durList = mkCDurList numList  
      tmpo = toRational (read tmpo' :: Float)
  (progname,_) <- getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer,DoubleBuffered] -- add a depth buffer
  createWindow "Diminuendo"
  windowSize $= Size 700 700  
  reshapeCallback $= Just reshape

--  play 4 beats at tempo to prepare  
  play $ tempo (tmpo/120) (timesM 4 (perc Cabasa qn))
  
  depthFunc $= Just Less -- specifies comparison function for DepthBuffer

  numB <- newIORef 1
  twist <- newIORef (0.0 :: GLfloat)
  counter <- newIORef (0.0 :: GLfloat)
--  keyboardMouseCallback $= Just (keyboardMouse angle position numB twist)
  idleCallback $= Just (idle)
  displayCallback $= (display numB counter twist numList durList tmpo)
  forkOS $ playCandB numList tmpo
  mainLoop
  
--use ghc -threaded -package GLUT Dimin.hs -o Dimin


--Peter lewis, 2012