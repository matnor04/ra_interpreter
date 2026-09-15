{-# LANGUAGE OverloadedStrings #-}
module CSVLoader (cargarCatalogoCSV) where

import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Text.Read (readMaybe)
import System.Directory (listDirectory)
import System.FilePath (dropExtension, takeExtension, (</>))
import Common (Valor(..), Tabla, Catalogo) -- Ajusta los imports según tu Common.hs

-- 1. Regla de Inferencia de Tipos
-- Si la celda es un número puro, lo convierte en 'N'. Si no, en 'Str'.
convertirCelda :: B8.ByteString -> Valor
convertirCelda bs = 
    let str = B8.unpack bs
    in case readMaybe str :: Maybe Int of
         Just numero -> N numero
         Nothing     -> Str str

-- 2. Procesar un archivo individual
cargarTablaCSV :: FilePath -> IO (Maybe Tabla)
cargarTablaCSV ruta = do
    csvData <- BL.readFile ruta
    -- decodeByName asume que la primera fila tiene los nombres de las columnas
    case decodeByName csvData of
        Left err -> do
            putStrLn $ "Error leyendo " ++ ruta ++ ": " ++ err
            return Nothing
        Right (_, v) -> do
            -- Convertimos la estructura de Cassava a nuestro [Map String Valor]
            let tabla = map convertirFila (V.toList v)
            return (Just tabla)
  where
    convertirFila :: HM.HashMap B8.ByteString B8.ByteString -> M.Map String Valor
    convertirFila hm = 
        M.fromList [ (B8.unpack k, convertirCelda v) | (k, v) <- HM.toList hm ]

-- 3. Cargar toda la carpeta
cargarCatalogoCSV :: FilePath -> IO Catalogo
cargarCatalogoCSV directorio = do
    archivos <- listDirectory directorio
    let archivosCSV = filter (\f -> takeExtension f == ".csv") archivos
    
    -- Leemos cada archivo y formamos una lista de pares ("Nombre", Tabla)
    tablas <- mapM (\arch -> do
        let nombreTabla = dropExtension arch -- Quita el ".csv"
        mTabla <- cargarTablaCSV (directorio </> arch)
        case mTabla of
            Just t  -> return [(nombreTabla, t)]
            Nothing -> return []
        ) archivosCSV
        
    -- M.fromList de una lista concatenada construye el Catálogo final
    return $ M.fromList (concat tablas)