module Square where
 
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

--vertify3 makes it easier to specify the coordinates for shape drawing
--I did not write this function -- credit to 
--haskell.org/haskellwiki/OpenGLTutorial2

vertify3 :: [(GLfloat,GLfloat,GLfloat)] -> IO ()
vertify3 verts = sequence_ $ map (\(a,b,c) -> vertex $ Vertex3 a b c) verts 

square w = renderPrimitive Quads $ vertify3
  [ (0, 0, 0), (w, -w, 0), (2*w, -w, 0), (3*w, 0, 0),
    (3*w, w, 0), (2*w, 2*w, 0), (w, 2*w, 0), (0, w, 0), (0, 0, 0)]  

{-
square w = renderPrimitive Quads $ vertify3
    [ (0, 0, 0), (w, 0, 0), (w, w, 0), (0, w, 0) ]
-}


--Peter Lewis, 2012