module Game.Util.BSP.Curves (checkForPatch) where

import qualified Graphics.Rendering.OpenGL as GL
import Foreign hiding (newArray)
import Data.Array.IArray
import Data.Array.IO
import Foreign.Storable

import Game.Types (BSPPatch(..), VertexArrays)

type VertTup = (Float, Float, Float, Float, Float, Float, Float)

-- given a face type return a list of patches if the facetype is 2.
-- Otherwise return an empty list.
checkForPatch :: Int -> Int -> (Int, Int) -> VertexArrays -> IO [BSPPatch]
checkForPatch faceType startVIndex (width, height) vertData
   | faceType == 2 = do
      patches <- createPatches vertData startVIndex width height 4
      return patches
   | otherwise = return []


-- Create control points for each patch.
-- Each patch has 9 control points.
getControlPointIndices :: Int -> Int -> Int -> [Int]
getControlPointIndices  i width height =
   concat [(create3x3ControlPoints x y)|
                    y <-[0..(((height-1) `div` 2)-1)],
                    x <-[0..(((width-1)  `div` 2)-1)]]
   where
         create3x3ControlPoints x y =
            [(i+((y*2*width)+(x*2))+(row*width)+point) |
                   row   <- [0..2],
                   point <- [0..2]]


-- Take a list of control points and split them into lists of 9
splitControlPoints :: [VertTup] -> [[VertTup]]
splitControlPoints [] = []
splitControlPoints tups = (take 9 tups):(splitControlPoints $ drop 9 tups)


-- gets the control points
getControlPoints :: VertexArrays -> Int -> Int -> Int -> IO [Array Int VertTup]
getControlPoints vertexData startIndex width height = do
   -- get the indices for the control points
   let indcs = getControlPointIndices  startIndex width  height
   -- get the vertices at those indices
   controlPoints <- mapM (readControlPoints vertexData) indcs
   -- divide the lists into arrays of 9 control points
   return $ map (listArray (0,8)) (splitControlPoints controlPoints)

-- reads the control point information from the vertex arrays
readControlPoints :: VertexArrays -> Int -> IO VertTup
readControlPoints  (vert, uv, lmuv, _, _) i = do
   x   <- peekElemOff vert  vertIndex     -- vertex coord
   y   <- peekElemOff vert (vertIndex+1)
   z   <- peekElemOff vert (vertIndex+2)
   u   <- peekElemOff uv    uvIndex       -- tex coord
   v   <- peekElemOff uv   (uvIndex+1)
   lmu <- peekElemOff lmuv  lmIndex       -- lightmap coord
   lmv <- peekElemOff lmuv (lmIndex+1)
   return (x,y,z,u,v,lmu,lmv)
   where
         vertIndex = i * 3
         uvIndex   = i * 2
         lmIndex   = i * 2


-- write the coordinate, texture coordinate and lightmap coordinates
-- for the cntrol points
writeControlPointData :: [VertTup] -> Int -> Ptr Float -> IO ()
writeControlPointData  [] _ _ = return()
writeControlPointData  ((a,b,c,d,e,f,g):rest) indx ptr = do
   let i = (indx*7)
   pokeElemOff ptr i       a
   pokeElemOff ptr (i+1) b
   pokeElemOff ptr (i+2) c
   pokeElemOff ptr (i+3) d
   pokeElemOff ptr (i+4) e
   pokeElemOff ptr (i+5) f
   pokeElemOff ptr (i+6) g
   writeControlPointData rest (indx+1) ptr

-- multiplies a set of floats by n
mul7 :: VertTup -> Float -> VertTup
mul7 (a,b,c,d,e,f,g) n = ((n*a),(n*b),(n*c),(n*d),(n*e),(n*f),(n*g))

-- adds to sets of floats together
add7 :: VertTup -> VertTup -> VertTup
add7 (u1,u2,u3,u4,u5,u6,u7) (v1,v2,v3,v4,v5,v6,v7) =
            (u1+v1,u2+v2,u3+v3,u4+v4,u5+v5,u6+v6,u7+v7)


-- create a set of patches
createPatches :: (Ptr Float, Ptr Float, Ptr Float, Ptr Float, Ptr Word8) ->
   Int -> Int -> Int -> Int -> IO [BSPPatch]
createPatches vertData startVert width height tesselation = do
   controlPoints <- getControlPoints vertData startVert width height
   patches <- mapM (createPatch tesselation) controlPoints
   return patches


createPatch :: Int -> Array Int VertTup -> IO BSPPatch
createPatch tesselation controlPoints  = do
   ptr <- mallocBytes (((tesselation+1)*(tesselation+1))*28)
   createPatch' tesselation ptr controlPoints
   createPatch'' tesselation ptr controlPoints
   (numiptr,iptrptr)<- generateIndices tesselation
   return BSPPatch { _patchLOD    = tesselation
                   , _patchPtr    = ptr
                   , _indexPtrPtr = iptrptr
                   , _numIndexPtr = numiptr }


createPatch' :: Int ->  Ptr Float -> Array Int VertTup -> IO()
createPatch' tess ptr arr = do
   let patchVerts = map (bezier tess (arr!0) (arr!3) (arr!6)) [0..tess]
   writeControlPointData patchVerts 0 ptr


createPatch'' :: Int ->  Ptr Float -> Array Int VertTup -> IO ()
createPatch'' tess ptr arr = do
   mapM_ (createPatch''' tess ptr arr) [1..tess]


createPatch''' :: Int -> Ptr Float -> Array Int VertTup -> Int -> IO ()
createPatch''' tess ptr arr u = do
   let tup1 = bezier tess (arr!0) (arr!1) (arr!2) u
   let tup2 = bezier tess (arr!3) (arr!4) (arr!5) u
   let tup3 = bezier tess (arr!6) (arr!7) (arr!8) u
   let patchVerts = map (bezier tess tup1 tup2 tup3) [0..tess]
   writeControlPointData patchVerts 0 (plusPtr ptr (((tess+1)*u)*28))


bezier :: Int -> VertTup -> VertTup -> VertTup -> Int -> VertTup
bezier tes cp1 cp2 cp3 i = add7 (add7 d1 d2) d3
   where
         d1 = mul7 cp1 ((1-p)*(1-p))
         d2 = mul7 cp2 ((1-p)*p*2)
         d3 = mul7 cp3 (p*p)
         p  = (realToFrac i)/(realToFrac tes)


-- generate indices
generateIndices :: Int -> IO (Ptr GL.GLsizei, Ptr (Ptr GL.GLint))
generateIndices tess = do
   indexArray <- newArray (0,((tess*(tess+1)*2)-1)) 0
   let pt1 = [ ((((row*(tess+1))+point)*2)+1, fromIntegral ((row*(tess+1))+point)) |
                         row<-[0..(tess-1)],
                         point<-[0..tess]]
   let pt2 = [ ((((row*(tess+1))+point)*2), fromIntegral (((row+1)*(tess+1))+point)) |
                         row<-[0..(tess-1)],
                         point<-[0..tess]]
   mapM_ (writeIndices indexArray) pt1
   mapM_ (writeIndices indexArray) pt2
   indexList <- (getElems indexArray)
   indexPtr <- mallocBytes ((tess * (tess+1)*2) * (sizeOf (undefined :: GL.GLint)))
   pokeArray indexPtr indexList
   numArrayIndicesPtr <- mallocBytes (tess * (sizeOf (undefined :: GL.GLsizei)))
   pokeArray numArrayIndicesPtr (map (\_->(fromIntegral (2*(tess+1)))) [0..(tess-1)])
   indexptrptr <- mallocBytes (tess * (sizeOf (undefined :: Ptr GL.GLint)))
   let ptrPtr = map (plusPtr indexPtr) [((sizeOf (undefined :: GL.GLint)) * (row*2*(tess+1))) | row <-[0..(tess-1)]]
   pokeArray indexptrptr ptrPtr
   return (numArrayIndicesPtr, indexptrptr)


-- writes the indices to memory
writeIndices :: IOUArray Int GL.GLint -> (Int,GL.GLint) -> IO ()
writeIndices indcs (pos,content) = writeArray indcs pos content
