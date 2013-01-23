Diminuendo
==========

A program for generating self-similar music and visualizations (Senior project, Yale U)

It uses the Euterpea music library for Haskell which can be found at:
http://haskell.cs.yale.edu/euterpea-2/download/

In addition, you will need to have OpenGL and HOpenGL (Haskell OpenGL) installed to compile. 
http://www.haskell.org/haskellwiki/Opengl

See the docs titled "Diminuendo Report" for a description of the project!

Finally, when compiling, be SURE to use "-threaded" :
ghc -threaded -package GLUT Dimin.hs -o Dimin
