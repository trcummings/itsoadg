module Game.Util.BSP.Frustum where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear                    as L

import Game.Types (ProjectionMatrix(..), ViewMatrix(..))


type FPlane = L.V4 Float
type Frustum = ( FPlane
               , FPlane
               , FPlane
               , FPlane
               , FPlane
               , FPlane )


normalisePlane :: FPlane -> FPlane
normalisePlane (L.V4 x y z d) =
  let reciMag = (1 / (sqrt(x * x + y * y + z * z)))
  in L.V4 (x * reciMag) (y * reciMag) (z * reciMag) (d * reciMag)


-- gets the frustum from the current view
getFrustum :: (ProjectionMatrix, ViewMatrix) -> IO Frustum
getFrustum (ProjectionMatrix projMatrix, ViewMatrix viewMatrix) = do
  let (L.V4 (L.V4 m00 m01 m02 m03 )
            (L.V4 m10 m11 m12 m13 )
            (L.V4 m20 m21 m22 m23 )
            (L.V4 m30 m31 m32 m33 )) = viewMatrix

      (L.V4 (L.V4 p00 p01 p02 p03 )
            (L.V4 p10 p11 p12 p13 )
            (L.V4 p20 p21 p22 p23 )
            (L.V4 p30 p31 p32 p33 )) = projMatrix

      clip00  = m00*p00 + m01*p10 + m02*p20 + m03*p30
      clip01  = m00*p01 + m01*p11 + m02*p21 + m03*p31
      clip02  = m00*p02 + m01*p12 + m02*p22 + m03*p32
      clip03  = m00*p03 + m01*p13 + m02*p23 + m03*p33

      clip10  = m10*p00 + m11*p10 + m12*p20 + m13*p30
      clip11  = m10*p01 + m11*p11 + m12*p21 + m13*p31
      clip12  = m10*p02 + m11*p12 + m12*p22 + m13*p32
      clip13  = m10*p03 + m11*p13 + m12*p23 + m13*p33

      clip20  = m20*p00 + m21*p10 + m22*p20 + m23*p30
      clip21  = m20*p01 + m21*p11 + m22*p21 + m23*p31
      clip22  = m20*p02 + m21*p12 + m22*p22 + m23*p32
      clip23  = m20*p03 + m21*p13 + m22*p23 + m23*p33

      clip30  = m30*p00 + m31*p10 + m32*p20 + m33*p30
      clip31  = m30*p01 + m31*p11 + m32*p21 + m33*p31
      clip32  = m30*p02 + m31*p12 + m32*p22 + m33*p32
      clip33  = m30*p03 + m31*p13 + m32*p23 + m33*p33

      rightX  = clip03 - clip00
      rightY  = clip13 - clip10
      rightZ  = clip23 - clip20
      rightD  = clip33 - clip30

      leftX   = clip03 + clip00
      leftY   = clip13 + clip10
      leftZ   = clip23 + clip20
      leftD   = clip33 + clip30

      bottomX = clip03 + clip01
      bottomY = clip13 + clip11
      bottomZ = clip23 + clip21
      bottomD = clip33 + clip31

      topX    = clip03 - clip01
      topY    = clip13 - clip11
      topZ    = clip23 - clip21
      topD    = clip33 - clip31

      backX   = clip03 - clip02
      backY   = clip13 - clip12
      backZ   = clip23 - clip22
      backD   = clip33 - clip32

      frontX  = clip03 + clip02
      frontY  = clip13 + clip12
      frontZ  = clip23 + clip22
      frontD  = clip33 + clip32

         -- right plane
  return ( normalisePlane (L.V4 rightX  rightY  rightZ  rightD)
         -- left plane
         , normalisePlane (L.V4 leftX   leftY   leftZ   leftD)
         -- bottom plane
         , normalisePlane (L.V4 bottomX bottomY bottomZ bottomD)
         -- top plane
         , normalisePlane (L.V4 topX    topY    topZ    topD)
         -- back plane
         , normalisePlane (L.V4 backX   backY   backZ   backD)
         -- front plane
         , normalisePlane (L.V4 frontX  frontY  frontZ  frontD) )


-- tests if a box intersects a plane
testBox :: L.V3 Float -> L.V3 Float -> FPlane -> Bool
testBox (L.V3 x y z) (L.V3 x2 y2 z2) (L.V4 a b c d)
   | (a * x  + b * y  + c * z  + d > 0) = True
   | (a * x2 + b * y  + c * z  + d > 0) = True
   | (a * x  + b * y2 + c * z  + d > 0) = True
   | (a * x2 + b * y2 + c * z  + d > 0) = True
   | (a * x  + b * y  + c * z2 + d > 0) = True
   | (a * x2 + b * y  + c * z2 + d > 0) = True
   | (a * x  + b * y2 + c * z2 + d > 0) = True
   | (a * x2 + b * y2 + c * z2 + d > 0) = True
   | otherwise = False


-- tests if an AABB lies within a frustum
boxInFrustum :: Frustum -> L.V3 Float -> L.V3 Float -> Bool
boxInFrustum (a,b,c,d,e,f) mn mx
   |     not(test a)
      || not(test b)
      || not(test c)
      || not(test d)
      || not(test e)
      || not(test f) = False
   | otherwise       = True
   where test = testBox mn mx
