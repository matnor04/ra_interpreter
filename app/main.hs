{-# LANGUAGE OverloadedStrings #-}
module Main where

import CSVLoader (cargarCatalogoCSV)
import Web.Scotty
import qualified Data.Map as M
import Data.IORef (newIORef, readIORef, writeIORef)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Text.Parsec (parse)

-- modulos originales
import Eval 
import Parser
import Common


-- http://localhost:3000/studio
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