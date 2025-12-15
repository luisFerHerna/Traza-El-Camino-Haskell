module Main where

import Juego
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as LBS (putStrLn)
import System.IO (hFlush, stdout, hSetBuffering, BufferMode(..))
import Data.List.Split (splitOn)
import System.Random (randomRIO)
import Text.Read (readMaybe)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    juegoInicial <- generarNivel 1 3
    loop juegoInicial

-- ============================================================================
-- GENERACIÓN DE NIVEL CON "ZONA SEGURA" SUPERIOR
-- ============================================================================
generarNivel :: Int -> Int -> IO Juego
generarNivel nNivel nVidas = do
    -- X: Mantenemos los márgenes laterales
    inicioX <- randomRIO (2, 5)   :: IO Int
    
    -- Y (Altura): CAMBIO IMPORTANTE
    -- Generamos entre 5 y 22. 
    -- Las filas 0, 1, 2, 3, 4 quedan reservadas para la interfaz (Score/Vidas)
    inicioY <- randomRIO (5, 22)  :: IO Int

    -- Meta en el lado derecho
    metaX   <- randomRIO (26, 30) :: IO Int
    metaY   <- randomRIO (5, 22)  :: IO Int -- También respeta el margen superior

    let inicio = (inicioX, inicioY)
    let meta = (metaX, metaY)
    let numObstaculos = if nNivel == 1 then 0 else (nNivel - 1) * 3

    obs <- generarObstaculos numObstaculos inicio meta []
    return $ crearJuego inicio meta obs nVidas nNivel

generarObstaculos :: Int -> Coord -> Coord -> [Coord] -> IO [Coord]
generarObstaculos 0 _ _ acc = return acc
generarObstaculos n start end acc = do
    rx <- randomRIO (6, 25) :: IO Int
    
    -- Y (Altura) de obstáculos: También entre 5 y 22
    ry <- randomRIO (5, 22) :: IO Int
    
    let pos = (rx, ry)
    if pos == start || pos == end || pos `elem` acc
        then generarObstaculos n start end acc
        else generarObstaculos (n - 1) start end (pos : acc)

loop :: Juego -> IO ()
loop estadoActual = do
    linea <- getLine
    let partes = splitOn " " linea
    case partes of
        ["RESET_PATH"] -> do
            let nuevoEstado = limpiarCamino estadoActual
            responder nuevoEstado
            loop nuevoEstado

        ["DIBUJAR", xStr, yStr] -> do
            case (readMaybe xStr, readMaybe yStr) of
                (Just x, Just y) -> do
                    let nuevoEstado = agregarDibujo (x, y) estadoActual
                    responder nuevoEstado
                    loop nuevoEstado
                _ -> loop estadoActual

        ["INICIAR"] -> do
            let nuevoEstado = intentarIniciar estadoActual
            responder nuevoEstado
            loop nuevoEstado

        ["TICK"] -> do
            let nuevoEstado = avanzarCoche estadoActual
            responder nuevoEstado
            loop nuevoEstado

        ["SIGUIENTE_NIVEL"] -> do
            nuevoJuego <- generarNivel (nivelActual estadoActual + 1) (vidas estadoActual)
            responder nuevoJuego
            loop nuevoJuego

        ["REINTENTAR_O_PERDER"] -> do
            let vidasRestantes = vidas estadoActual - 1
            if vidasRestantes > 0
                then do
                    nuevoJuego <- generarNivel (nivelActual estadoActual) vidasRestantes
                    responder nuevoJuego
                    loop nuevoJuego
                else do
                    let juegoGameOver = estadoActual { estado = GameOver, vidas = 0 }
                    responder juegoGameOver
                    loop juegoGameOver

        ["REINICIAR_COMPLETO"] -> do
            juegoNuevo <- generarNivel 1 3
            responder juegoNuevo
            loop juegoNuevo

        ["QUIT"] -> return ()
        _ -> loop estadoActual

responder :: Juego -> IO ()
responder st = do
    LBS.putStrLn $ encode st
    hFlush stdout