
module Algebra where

-- Aqui vem a definição das entidades algébricas que precisarão ser utilizadas
data Vector3D = Coord (Double,  Double,  Double)
              | Color (Integer, Integer, Integer)
    deriving (Show, Eq)

class Vector a where
    ($+)      :: Vector3D -> Vector3D -> Vector3D
    ($-)      :: Vector3D -> Vector3D -> Vector3D
    ($*)      :: Double   -> Vector3D -> Vector3D
    ($.)      :: Vector3D -> Vector3D -> Double
    ($/)      :: Vector3D -> Double   -> Vector3D
    normalize :: Vector3D -> Vector3D

instance Vector Vector3D where
    ($+) :: Vector3D -> Vector3D -> Vector3D
    ($+) (Coord (a, b, c)) (Coord (d, e, f)) = Coord (a + d, b + e, c + f)

    ($-) :: Vector3D -> Vector3D -> Vector3D
    ($-) x (Coord (a, b, c)) = x $+ (Coord (-a, -b, -c))

    ($*) :: Double -> Vector3D -> Vector3D
    ($*) x (Coord (a, b, c)) = Coord (x * a, x * b, x * c)

    ($.) :: Vector3D -> Vector3D -> Double
    ($.) (Coord (a, b, c)) (Coord (d, e, f)) = a * d + b * e + c * f

    ($/) :: Vector3D -> Double -> Vector3D
    ($/) x y = (1 / y) $* x

    normalize :: Vector3D -> Vector3D
    normalize x = x $/ (x $. x)
