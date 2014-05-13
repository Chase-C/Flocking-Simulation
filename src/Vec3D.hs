module Vec3D where

newtype Vec3D = Vec3D (Float, Float, Float) deriving (Show, Eq)

zeroVec :: Vec3D
zeroVec = Vec3D (0, 0, 0)

vX, vY, vZ :: Vec3D -> Float
vX (Vec3D x _ _) = x
vY (Vec3D _ y _) = y
vZ (Vec3D _ _ z) = z

vAdd, vSub, vCross :: Vec3D -> Vec3D -> Vec3D
vAdd   (Vec3D (x1, y1, z1)) (Vec3D (x2, y2, z2)) = Vec3D (x1 + x2, y1 + y2, z1 + z2)
vSub   (Vec3D (x1, y1, z1)) (Vec3D (x2, y2, z2)) = Vec3D (x1 - x2, y1 - y2, z1 - z2)
vCross (Vec3D (x1, y1, z1)) (Vec3D (x2, y2, z2)) = Vec3D (s1, s2, s3)
    where s1 = (y1 * z2) - (z1 * y2)
          s2 = (z1 * x2) - (x1 * z2)
          s3 = (x1 * y2) - (y1 * x2)

vDot :: Vec3D -> Vec3D -> Float
vDot (Vec3D (x1, y1, z1)) (Vec3D (x2, y2, z2)) = (x1 * x2) + (y1 * y2) + (z1 * z2)

vLen, vSqLen :: Vec3D -> Float
vSqLen (Vec3D (x, y, z)) = (x * x) + (y * y) + (z * z)
vLen                     = sqrt . vSqLen

vScale, vScaleTo, vClamp :: Vec3D -> Float -> Vec3D
vScale (Vec3D (0, 0, 0)) _ = zeroVec
vScale (Vec3D (x, y, z)) n = Vec3D (n * x, n * y, n * z)
vClamp (Vec3D (0, 0, 0)) _ = zeroVec
vClamp vec               n = if sqlen > (n * n) then vScale vec (n / (sqrt sqlen)) else vec
                             where sqlen = vSqLen vec
vScaleTo vec             n = vScale vec (n / (vLen vec))

vNorm :: Vec3D -> Vec3D
vNorm vec = vScale vec (1.0 / (vLen vec))

vAdd3 :: Vec3D -> Vec3D -> Vec3D -> Vec3D
vAdd3 v1 v2 v3 = vAdd (vAdd v1 v2) v3

vAdd4 :: Vec3D -> Vec3D -> Vec3D -> Vec3D -> Vec3D
vAdd4 v1 v2 v3 v4 = vAdd (vAdd3 v1 v2 v3) v4
