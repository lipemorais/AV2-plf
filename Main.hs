
module Main where

import Raytracer
import Algebra


-- Esta função recebe como parâmetros a largura, a altura e uma lista de pixels (R,G,B) que compõem
-- uma imagem e então gera uma string que representa um arquivo de imagem no formato PPM
create_ppm :: Double -> Double -> [Vector3D] -> String
create_ppm w h pixels = "P3\n" ++ show (round w) ++ " " ++ show (round h) ++ "\n255\n" ++ (foldr ((++) . join) "" pixels)
    where join (Color (r, g, b)) = show r ++ " " ++ show g ++ " " ++ show b ++ "\n"

-- Salva uma string em um arquivo texto
save_ppm :: String -> String -> IO ()
save_ppm filename text = writeFile filename text

objeto1 = Sphere (Coord (0,0,-300)) 200

main = save_ppm "teste.ppm" (create_ppm hres vres (build_scene all_rays objeto1))