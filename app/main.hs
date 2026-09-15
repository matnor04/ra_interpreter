{-# LANGUAGE OverloadedStrings #-}
module Main where

import CSVLoader (cargarCatalogoCSV)
import Web.Scotty
import qualified Data.Map as M
import Data.IORef (newIORef, readIORef, writeIORef)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Text.Parsec (parse)

-- Tus módulos originales
import Eval 
import Parser
import Common

{- 
  NOTA: Asegúrate de tener importado Data.Aeson en Common.hs y 
  de haber agregado la instancia ToJSON para tu tipo Valor:
  
  instance ToJSON Valor where
      toJSON (N i) = Number (fromIntegral i)
      toJSON (Str s) = String (T.pack s)
-}

-- Tu catálogo hardcodeado original (Sin modificaciones)
spjCatalog :: Catalogo
spjCatalog = M.fromList 
    [ ("S", [ M.fromList [("SID", Str "S1"), ("SNOMBRE", Str "Salazar"), ("SITUACION", N 20), ("SCIUDAD", Str "Londres")] 
            , M.fromList [("SID", Str "S2"), ("SNOMBRE", Str "Jaimes"),  ("SITUACION", N 10), ("SCIUDAD", Str "Paris")] 
            , M.fromList [("SID", Str "S3"), ("SNOMBRE", Str "Bernal"),  ("SITUACION", N 30), ("SCIUDAD", Str "Paris")] 
            , M.fromList [("SID", Str "S4"), ("SNOMBRE", Str "Corona"),  ("SITUACION", N 20), ("SCIUDAD", Str "Londres")] 
            , M.fromList [("SID", Str "S5"), ("SNOMBRE", Str "Aldana"),  ("SITUACION", N 30), ("SCIUDAD", Str "Atenas")] 
            ])
    , ("P", [ M.fromList [("PID", Str "P1"), ("PNOMBRE", Str "Tuerca"),    ("COLOR", Str "Rojo"),  ("PESO", N 12), ("PCIUDAD", Str "Londres")] 
            , M.fromList [("PID", Str "P2"), ("PNOMBRE", Str "Perno"),     ("COLOR", Str "Verde"), ("PESO", N 17), ("PCIUDAD", Str "Paris")] 
            , M.fromList [("PID", Str "P3"), ("PNOMBRE", Str "Burlete"),   ("COLOR", Str "Azul"),  ("PESO", N 17), ("PCIUDAD", Str "Roma")] 
            , M.fromList [("PID", Str "P4"), ("PNOMBRE", Str "Burlete"),   ("COLOR", Str "Rojo"),  ("PESO", N 14), ("PCIUDAD", Str "Londres")] 
            , M.fromList [("PID", Str "P5"), ("PNOMBRE", Str "Leva"),      ("COLOR", Str "Azul"),  ("PESO", N 12), ("PCIUDAD", Str "Paris")] 
            , M.fromList [("PID", Str "P6"), ("PNOMBRE", Str "Engranaje"), ("COLOR", Str "Rojo"),  ("PESO", N 19), ("PCIUDAD", Str "Londres")] 
            ])
    , ("J", [ M.fromList [("JID", Str "J1"), ("JNOMBRE", Str "Clasificador"), ("JCIUDAD", Str "Paris")] 
            , M.fromList [("JID", Str "J2"), ("JNOMBRE", Str "Perforadora"),  ("JCIUDAD", Str "Roma")] 
            , M.fromList [("JID", Str "J3"), ("JNOMBRE", Str "Lectora"),      ("JCIUDAD", Str "Atenas")] 
            , M.fromList [("JID", Str "J4"), ("JNOMBRE", Str "Consola"),      ("JCIUDAD", Str "Atenas")] 
            , M.fromList [("JID", Str "J5"), ("JNOMBRE", Str "Compaginador"), ("JCIUDAD", Str "Londres")] 
            , M.fromList [("JID", Str "J6"), ("JNOMBRE", Str "Terminal"),     ("JCIUDAD", Str "Oslo")] 
            , M.fromList [("JID", Str "J7"), ("JNOMBRE", Str "Cinta"),        ("JCIUDAD", Str "Londres")] 
            ])
    , ("SPJ", [ M.fromList [("SID", Str "S1"), ("PID", Str "P1"), ("JID", Str "J1"), ("CANT", N 200)] 
              , M.fromList [("SID", Str "S1"), ("PID", Str "P1"), ("JID", Str "J4"), ("CANT", N 700)] 
              , M.fromList [("SID", Str "S2"), ("PID", Str "P3"), ("JID", Str "J1"), ("CANT", N 400)] 
              , M.fromList [("SID", Str "S2"), ("PID", Str "P3"), ("JID", Str "J2"), ("CANT", N 200)] 
              , M.fromList [("SID", Str "S2"), ("PID", Str "P3"), ("JID", Str "J3"), ("CANT", N 200)] 
              , M.fromList [("SID", Str "S2"), ("PID", Str "P3"), ("JID", Str "J4"), ("CANT", N 500)] 
              , M.fromList [("SID", Str "S2"), ("PID", Str "P3"), ("JID", Str "J5"), ("CANT", N 600)] 
              , M.fromList [("SID", Str "S2"), ("PID", Str "P3"), ("JID", Str "J6"), ("CANT", N 400)] 
              , M.fromList [("SID", Str "S2"), ("PID", Str "P3"), ("JID", Str "J7"), ("CANT", N 800)] 
              , M.fromList [("SID", Str "S2"), ("PID", Str "P5"), ("JID", Str "J2"), ("CANT", N 100)] 
              , M.fromList [("SID", Str "S3"), ("PID", Str "P3"), ("JID", Str "J1"), ("CANT", N 200)] 
              , M.fromList [("SID", Str "S3"), ("PID", Str "P4"), ("JID", Str "J2"), ("CANT", N 500)] 
              , M.fromList [("SID", Str "S4"), ("PID", Str "P6"), ("JID", Str "J3"), ("CANT", N 300)] 
              , M.fromList [("SID", Str "S4"), ("PID", Str "P6"), ("JID", Str "J7"), ("CANT", N 300)] 
              , M.fromList [("SID", Str "S5"), ("PID", Str "P2"), ("JID", Str "J2"), ("CANT", N 200)] 
              , M.fromList [("SID", Str "S5"), ("PID", Str "P2"), ("JID", Str "J4"), ("CANT", N 100)] 
              , M.fromList [("SID", Str "S5"), ("PID", Str "P5"), ("JID", Str "J5"), ("CANT", N 500)] 
              , M.fromList [("SID", Str "S5"), ("PID", Str "P5"), ("JID", Str "J7"), ("CANT", N 100)] 
              , M.fromList [("SID", Str "S5"), ("PID", Str "P1"), ("JID", Str "J4"), ("CANT", N 100)] 
              , M.fromList [("SID", Str "S5"), ("PID", Str "P3"), ("JID", Str "J4"), ("CANT", N 200)] 
              , M.fromList [("SID", Str "S5"), ("PID", Str "P4"), ("JID", Str "J4"), ("CANT", N 800)] 
              , M.fromList [("SID", Str "S5"), ("PID", Str "P5"), ("JID", Str "J4"), ("CANT", N 400)] 
              , M.fromList [("SID", Str "S5"), ("PID", Str "P6"), ("JID", Str "J4"), ("CANT", N 500)] 
              ])
    ]

main :: IO ()
main = do
    putStrLn "Iniciando servidor web y montando base de datos..."
    
    catInicial <- cargarCatalogoCSV "tablas"
    estadoGlobal <- newIORef catInicial
    
    scotty 3000 $ do
        -- 1. Servimos el HTML
        get "/studio" $ file "static/app.html"
        
        get "/api/catalog" $ do
            catActual <- liftIO $ readIORef estadoGlobal
            json (M.keys catActual)

        -- 2. Endpoint de la API
        post "/api/ejecutar" $ do
            input <- param "consulta"
            catActual <- liftIO $ readIORef estadoGlobal
            
            -- Interceptamos comandos del sistema igual que en tu CLI
            case input of
                "getcat" -> do
                    let tablas = M.keys catActual
                    json $ object ["status" .= ("info" :: String), "msg" .= ("Tablas en memoria: " ++ show tablas)]
                
                "reset" -> do
                    liftIO $ writeIORef estadoGlobal spjCatalog
                    json $ object ["status" .= ("info" :: String), "msg" .= ("Catálogo reiniciado al estado inicial." :: String)]
                
                _ -> do
                    -- Usamos tu flujo de Parsec y Eval original
                    case parse parseStmt "web" input of 
                        Left err -> 
                            json $ object ["status" .= ("error" :: String), "msg" .= ("Error en parsing: " ++ show err)]
                        
                        Right stmt ->
                            case evalStmt stmt catActual of 
                                Impos msj -> 
                                    json $ object ["status" .= ("error" :: String), "msg" .= ("Error: " ++ msj)]
                                
                                Undef nom -> 
                                    json $ object ["status" .= ("error" :: String), "msg" .= ("Tabla no encontrada: " ++ nom)]
                                
                                Res (mTabla, catNuevo) -> do
                                    -- Actualizamos el estado global en RAM con el nuevo catálogo
                                    liftIO $ writeIORef estadoGlobal catNuevo
                                    
                                    case mTabla of 
                                        Just tabla -> 
                                            json $ object ["status" .= ("ok" :: String), "data" .= tabla]
                                        Nothing -> 
                                            json $ object ["status" .= ("info" :: String), "msg" .= ("Variable asignada correctamente." :: String)]