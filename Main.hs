module Main where

import Juego
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as LBS (putStrLn)
import System.IO (hFlush, stdout, hSetBuffering, BufferMode(..))
import Control.Monad (forever, replicateM)
import Data.List.Split (splitOn)
import System.Random (randomRIO)
import Text.Read (readMaybe)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    -- Iniciamos Nivel 1 con 3 vidas
    juegoInicial <- generarNivel 1 3
    loop juegoInicial

-- Genera un nuevo estado de juego con posiciones aleatorias y obstaculos
generarNivel :: Int -> Int -> IO Juego
generarNivel nNivel nVidas = do
    -- Coordenadas Inicio y Meta (RANGOS AJUSTADOS para evitar bordes)
    inicioX <- randomRIO (3, 5)   :: IO Int
    inicioY <- randomRIO (3, 18)  :: IO Int
    metaX   <- randomRIO (28, 30) :: IO Int
    metaY   <- randomRIO (3, 18)  :: IO Int

    let inicio = (inicioX, inicioY)
    let meta = (metaX, metaY)

    -- Calcular número de obstáculos: Nivel 1 = 0, Nivel 2 = 3, Nivel 3 = 6...
    let numObstaculos = if nNivel == 1 then 0 else (nNivel - 1) * 3

    -- Generar obstaculos que no caigan en inicio ni meta
    obs <- generarObstaculos numObstaculos inicio meta []

    return $ crearJuego inicio meta obs nVidas nNivel

-- Función recursiva para generar coordenadas únicas
generarObstaculos :: Int -> Coord -> Coord -> [Coord] -> IO [Coord]
generarObstaculos 0 _ _ acc = return acc
generarObstaculos n start end acc = do
    rx <- randomRIO (8, 25) :: IO Int
    ry <- randomRIO (3, 18) :: IO Int -- Rango ajustado
    let pos = (rx, ry)
    if pos == start || pos == end || pos `elem` acc
        then generarObstaculos n start end acc -- Reintentar si choca
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

        -- COMANDOS NUEVOS PARA GESTIÓN DE NIVELES Y VIDAS --

        ["SIGUIENTE_NIVEL"] -> do
            -- Aumentar nivel, mantener vidas
            nuevoJuego <- generarNivel (nivelActual (estadoActual) + 1) (vidas estadoActual)
            responder nuevoJuego
            loop nuevoJuego

        ["REINTENTAR_O_PERDER"] -> do
            let vidasRestantes = vidas estadoActual - 1
            if vidasRestantes > 0
                then do
                    -- Mismo nivel (o nuevo mapa del mismo nivel), una vida menos
                    nuevoJuego <- generarNivel (nivelActual estadoActual) vidasRestantes
                    responder nuevoJuego
                    loop nuevoJuego
                else do
                    -- Game Over: Enviamos estado especial
                    let juegoGameOver = estadoActual { estado = GameOver, vidas = 0 }
                    responder juegoGameOver
                    loop juegoGameOver

        ["REINICIAR_COMPLETO"] -> do
            -- Volver a Nivel 1 con 3 vidas
            juegoNuevo <- generarNivel 1 3
            responder juegoNuevo
            loop juegoNuevo

        ["QUIT"] -> return ()
        _ -> loop estadoActual

responder :: Juego -> IO ()
responder st = do
    LBS.putStrLn $ encode st
    hFlush stdout