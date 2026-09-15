module PPAR (ppTabla) where

import Common
import qualified Data.Map as M
import Data.List (intercalate, transpose)

-- conversión a string
showVal :: Valor -> String
showVal (N i) = show i
showVal (Str s) = s 

-- máximo ancho por columna
maxWidth :: [Atributo] -> Tabla -> [Int]
maxWidth atrs tabla = map colWidth atrs
  where
    colWidth header = maximum (length header : map (valLength header) tabla)
    valLength header row = length (showVal (row M.! header))

-- string con padding
pad :: Int -> String -> String
pad n s = s ++ replicate (n - length s) ' '

-- escribir separadores tipo "+------+------+"
seps :: [Int] -> String
seps widths = "+" ++ concatMap (\w -> replicate (w + 2) '-' ++ "+") widths

-- función principal para transformar tabla a string
ppTabla :: Tabla -> String
ppTabla [] = "Empty Table."
ppTabla tabla = 
    let 
        atrs = M.keys (head tabla)
        
        widths = maxWidth atrs tabla
        
        -- separador horizontal
        seplines = seps widths
        
        -- formato la fila con los atributos
        headerRow = "|" ++ concat (zipWith (\w h -> " " ++ pad w h ++ " |") widths atrs)
        
        -- formato de filas con la información en sí
        formatR row = "|" ++ concat (zipWith (\w h -> " " ++ pad w (showVal (row M.! h)) ++ " |") widths atrs)
        info = map formatR tabla
    in 
        unlines $ [seplines, headerRow, seplines] ++ info ++ [seplines]