{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import CSVLoader (cargarCatalogoCSV, parsearTablaCSV)
import Web.Scotty
import qualified Data.Map as M
import Data.IORef (newIORef, readIORef, writeIORef)
import Control.Monad.IO.Class (liftIO)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, object, (.=))
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.ByteString.Lazy as BL
import System.FilePath ((</>))
import Data.List (isSuffixOf)
import Text.Parsec (parse)

-- modulos originales
import Eval 
import Parser
import Common

data UploadPayload = UploadPayload {
    nombre :: String,
    contenido :: TL.Text
} deriving (Show, Generic)

instance FromJSON UploadPayload

main :: IO ()
main = do
    putStrLn "Iniciando servidor web y montando base de datos..."
    
    catInicial <- cargarCatalogoCSV "tablas"
    estadoGlobal <- newIORef catInicial
    
    scotty 3000 $ do
        -- html principal
        get "/studio" $ file "static/app.html"
        
        get "/api/catalog" $ do
            catActual <- liftIO $ readIORef estadoGlobal
            json (M.keys catActual)

        -- subir tabla CSV
        post "/api/upload" $ do
            payload <- jsonData :: ActionM UploadPayload
            let nom = nombre payload
                cleanNom = if ".csv" `isSuffixOf` nom then take (length nom - 4) nom else nom
                csvBS = TLE.encodeUtf8 (contenido payload)
            
            if null cleanNom
                then json $ object ["status" .= ("error" :: String), "msg" .= ("El nombre de la tabla no puede estar vacío." :: String)]
                else case parsearTablaCSV csvBS of
                    Left err ->
                        json $ object ["status" .= ("error" :: String), "msg" .= ("Error al procesar CSV: " ++ err)]
                    Right tabla -> do
                        liftIO $ BL.writeFile ("tablas" </> (cleanNom ++ ".csv")) csvBS
                        catActual <- liftIO $ readIORef estadoGlobal
                        liftIO $ writeIORef estadoGlobal (M.insert cleanNom tabla catActual)
                        json $ object [
                            "status" .= ("ok" :: String),
                            "msg" .= ("Tabla '" ++ cleanNom ++ "' subida con éxito (" ++ show (length tabla) ++ " filas)."),
                            "nombre" .= cleanNom
                          ]

        -- api endpoint de consultas
        post "/api/ejecutar" $ do
            input <- param "consulta"
            catActual <- liftIO $ readIORef estadoGlobal
            
            case input of
                "getcat" -> do
                    let tablas = M.keys catActual
                    json $ object ["status" .= ("info" :: String), "msg" .= ("Tablas en memoria: " ++ show tablas)]
                
                "reset" -> do
                    liftIO $ writeIORef estadoGlobal catInicial
                    json $ object ["status" .= ("info" :: String), "msg" .= ("Catálogo reiniciado al estado inicial." :: String)]
                
                _ -> do
                    case parse (totParser parseStmt) "web" input of 
                        Left err -> 
                            json $ object ["status" .= ("error" :: String), "msg" .= ("Error en parsing: " ++ show err)]
                        
                        Right stmt ->
                            case evalStmt stmt catActual of 
                                Impos msj -> 
                                    json $ object ["status" .= ("error" :: String), "msg" .= ("Error: " ++ msj)]
                                
                                Undef nom -> 
                                    json $ object ["status" .= ("error" :: String), "msg" .= ("Tabla no encontrada: " ++ nom)]
                                
                                Res (mTabla, catNuevo) -> do
                                    liftIO $ writeIORef estadoGlobal catNuevo
                                    
                                    case mTabla of 
                                        Just tabla -> 
                                            json $ object ["status" .= ("ok" :: String), "data" .= tabla]
                                        Nothing -> 
                                            json $ object ["status" .= ("info" :: String), "msg" .= ("Variable asignada correctamente." :: String)]