
module Algebra where

-- Aqui vem a definição das entidades algébricas que precisarão ser utilizadas
data Vector3D = Coord (Double,  Double,  Double)
              | Color (Integer, Integer, Integer)
    deriving (Show, Eq)

class Vector a where
    ($+)      :: a -> a -> a
    ($-)      :: a -> a -> a
    ($*)      :: Double   -> a -> a
    ($.)      :: a -> a -> Double
    ($/)      :: a -> Double   -> a
    normalize :: a -> a
