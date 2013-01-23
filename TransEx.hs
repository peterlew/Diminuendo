module TransEx where

import Graphics.Rendering.OpenGL

type Transformation = (String, Float, (Float, Float, Float))
type TransformationList = [Transformation]

--here are a bunch of sample transformations. To make custom transformations,
--enter them here and change intMap1 to contain your transformations

tEmpt, t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, 
       t10, t11, t12, t13, t14 :: TransformationList

tEmpt = []
t0 = [("Translate", 0.0, (0.25, 0.25, 0.0)),
      ("Rotate", 90.0/8.0, (0.0, 0.0, 1.0))]
t1 = [("Rotate", 180.0, (1.0, -1.0, 0.0)),
      ("Rotate", 6.0*90.0/8.0, (0.0, 0.0, 1.0))]
t2 = []

---------------

t3 = [("Translate", 0.0, (0.5, 0.2, 0.0))]
t4 = [("Flip", 0.0, (0.0, 0.0, 0.0)),
      ("Rotate", -25, (0.0, 0.0, 1.0))]
t5 = [("Translate", 0.0, (-0.2, -0.5, 0.0))]
t6 = [("Translate", 0.0, (-0.3, 0.3, 0.0)),
      ("Rotate", 25, (0.0, 0.0, 1.0))]
      
---------------

t7 = [("Rotate", 90, (0.0, 0.0, 1.0))]
t8 = [("Flip", 1.0, (0.0, 0.0, 0.0))]
t9 = [("Rotate", -60, (0.0, 0.0, -1.0))]
t10 = [("Rotate", -30, (0.0, 0.0, -1.0))]

-----------------

t11 = [("Rotate", 45, (0.0, 0.0, 1.0))]
t12 = [("Flip", 1.0, (0.0, 0.0, 0.0))]
t13 = [("Translate", 0.0, (0.1, 0.05, 0.0)),
       ("Rotate", 45, (0.0, 0.0, 1.0))]
t14 = [("Rotate", -45, (0.0, 0.0, -1.0))] 


intToTrans :: [Int] -> [TransformationList] -> Int -> TransformationList
intToTrans (fi:is) (ft:ts) i = if fi == i then ft else intToTrans is ts i

intMap1 :: Int -> TransformationList
intMap1 = intToTrans [0, 1, 2, 3] [t11, t12, t13, t14]

--maybe we want to see what transformation is taking place. Let's make a map from
--transformations to Color3 colors

col0, col1, col2, col3, colInit,
 col4, col5, col6, col7 :: Color3 GLfloat

col0 = Color3 (1.0 :: GLfloat) 0.0 0.0 -- red
col1 = Color3 (0.0 :: GLfloat) 1.0 0.0 -- green
col2 = Color3 (0.0 :: GLfloat) 0.0 1.0 -- blue
col3 = Color3 (1.0 :: GLfloat) 1.0 0.0 -- yellow

-----

col4 = Color3 (224.0/255.0 :: GLfloat) (27.0/255.0) (106.0/255.0) -- pink
col5 = Color3 (34.0/255.0 :: GLfloat) (214.0/255.0) (184.0/255.0) -- teal
col6 = Color3 (219.0/255.0 :: GLfloat) (149.0/255.0) (35.0/255.0) -- peach
col7 = Color3 (81.0/255.0 :: GLfloat) (219.0/255.0) (35.0/255.0) -- lime

colInit = Color3 (1.0 :: GLfloat) 1.0 1.0 -- white

numToCol :: [Int] -> [Color3 GLfloat] -> Int -> Color3 GLfloat
numToCol (ft:ts) (fc:cs) t = if ft == t then fc else numToCol ts cs t

colMap1 :: Int -> Color3 GLfloat
colMap1 = numToCol [0, 1, 2, 3, -1] [col4, col5, col6, col7, colInit]

--and a reduce brightness feature

reduCol :: Color3 GLfloat -> Float -> Color3 GLfloat
reduCol (Color3 r g b) s = Color3 (s*r) (s*g) (s*b)

--for the current draw animation, we want a gradual color combine

colCombine :: Int -> Int -> Float -> Color3 GLfloat
colCombine n1 n2 c = 
    colAdd (reduCol (colMap1 n1) (1.0-c)) (reduCol (colMap1 n2) c)
  where 
    colAdd (Color3 r1 g1 b1) (Color3 r2 g2 b2) =
        Color3 (r1+r2) (g1+g2) (b1+b2)

--Rotations are "undone" after every transformation. Rotations from previous 
--transformations need to be added to future TransformationLists         
         
--This is perfect for a stack ... find a rotation, put it in the stack. Move
--along the line. Add each step, unload the stack and add any rotations to it

--just to be clear. the stack will contain rotations with the earliest ones at 
--the bottom, latest ones at the top. insertRotation will then put each rotation,
--starting at the top, directly in front of all other rotations in the transList.
--So, the first rotations will end up first, like we want


--myMod takes is a mod for floats which preserves sign 
--good for 540 degrees = 180 degrees, -370 degrees = -10 degrees, etc.
myMod :: Float -> Float -> Float
myMod x n = let mRed x n = if x < n then x else mRed (x - n) n
            in if x < 0 then (-1*mRed (-1*x) n) else mRed x n

--insert a rotation in a TransformationList            
insertRotation :: Transformation -> TransformationList -> TransformationList
insertRotation r [] = [r]
insertRotation r@(_,ramt,_) t@(t1@(nm,amt,vec):ts) = 
    if nm == "Rotate" then (nm,myMod (ramt + amt) 360,vec):ts
    else if nm == "Flip" then r:t 
         else t1 : insertRotation r ts

--add any rotations in a TransformationList to the rotation stack         
addToRotStack :: TransformationList -> [Transformation] -> [Transformation]
addToRotStack [] stk = stk
addToRotStack (t1@(nm,_,_):ts) stk = if nm == "Rotate" then addToRotStack ts (t1:stk)
                                     else addToRotStack ts stk
                        
--put all the rotations in the stack into a TransformationList                        
unloadRotStack :: TransformationList -> [Transformation] -> TransformationList
unloadRotStack ts [] = ts
unloadRotStack ts (s1:ss) = unloadRotStack (insertRotation s1 ts) ss

--go through a list of TransformationLists, add all previous rotations to each
fixRotations :: [TransformationList] -> [TransformationList]
fixRotations ts' = 
    let frh [] stk = []
        frh (t1:ts) stk = unloadRotStack t1 stk : frh ts (addToRotStack t1 stk)
    in frh ts' []
    
--undo whatever rotations were just performed        
undoRotations :: TransformationList -> IO ()    

undoRotations [] = return ()
undoRotations (t1@(s,a,c):ts) = 
    if s == "Rotate" then do
        rotate (-a) $ Vector3 0.0 0.0 1.0
        undoRotations ts
    else undoRotations ts
                   
--also, to get the animation working, we need to pass drawTListGrad a float
--representing what the last rotation is. (Remember, a transformation doesn't
--really know how much of a rotation it contains, just the cumulative rotation)

findRot :: TransformationList -> Float
findRot ts' = 
    let frh [] cur = cur
        frh ((s,a,p):ts) cur = case s of
                                "Rotate" -> frh ts (myMod (cur+a) 360.0)
                                otherwise -> frh ts cur
    in frh ts' 0.0
    
    
--Peter Lewis, 2012
                   





