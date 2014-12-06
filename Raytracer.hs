
module Raytracer where

import Algebra

-- Algumas definições de tipo
type Center    = Vector3D
type Direction = Vector3D
type Radius    = Double
type Origin    = Vector3D
type Normal    = Vector3D
type HitPoint  = Vector3D

hres         = 500
vres         = 500
pixel_size   = 1.0
zw           = 100.0
light_pos    = Coord (-600.0, -200.0, 300.0)
diffuse_coef = 80.0
gamma        = 8.0

-- Definição das estruturas de dados
data Object = Sphere Center Radius
    deriving (Show, Eq)

data Ray = Ray Origin Direction
    deriving (Show, Eq)

data RayData = NoHit
             | Shade Normal HitPoint
             deriving (Show, Eq)

-- Esta função faz o cálculo para verificar se algum raio atinge a esfera
calc :: Ray -> Object -> RayData
calc (Ray origin direction) (Sphere center radius) = if (disc > 0.0) then (Shade normal lhp) else NoHit
    where temp   = origin $- center
          a      = direction $. direction
          b      = 2.0 * (temp $. direction)
          c      = (temp $. temp) - radius * radius
          disc   = b * b - 4.0 * a * c
          e      = sqrt disc
          denom  = 2.0 * a
          t1     = (-b - e) / denom
          t2     = (-b + e) / denom
          t      = if (t1 > 0.0) then t1 else t2
          normal = temp $+ (t $* direction) $/ radius
          lhp    = origin $+ (t $* direction)

-- Calcula a posição do raio no mundo virtual a partir do índice do pixel
get_x :: Double -> Double
get_x c = pixel_size * (c - 0.5 * (hres - 1.0))

get_y :: Double -> Double
get_y c = pixel_size * (c - 0.5 * (vres - 1.0))

-- Calcula a cor do pixel a partir do ângulo de incidência do raio
get_color :: RayData -> Vector3D -> Vector3D
get_color NoHit _ = Coord (0.0, 0.0, 0.0)
get_color (Shade normal hp) light = Coord (min 255 ((max 0 (normal $. l)) * 255 * diffuse_coef * gamma), 0.0, 0.0)
    where l = normalize (light $- hp)

-- Traça um raio
trace_ray :: Ray -> Object -> Vector3D
trace_ray r o = get_color (calc r o) light_pos

-- Gera uma estrutura de dados com todos os raios
all_x = map (\x -> pixel_size * (x - 0.5 * (hres - 1.0))) [0..(hres - 1)]
all_y = map (\y -> pixel_size * (y - 0.5 * (vres - 1.0))) [0..(vres - 1)]
all_rays = [Coord (x, y, zw) | x <- all_x, y <- all_y]

-- Constrói a cena
build_scene :: [Vector3D] -> Object -> [Vector3D]
build_scene rays obj = map (\x -> coord_to_color (trace_ray (Ray x (Coord (0.0, 0.0, -1.0))) obj)) rays

-- Converte a cor de Double para Integer
coord_to_color :: Vector3D -> Vector3D
coord_to_color (Coord (x, y, z)) = (Color (round x, round y, round z))
