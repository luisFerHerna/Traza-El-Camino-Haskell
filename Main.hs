module Main where

import Juego
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as LBS (putStrLn)
import System.IO (hFlush, stdout, hSetBuffering, BufferMode(..))
import Control.Monad (forever, replicateM)
import Data.List.Split (splitOn)
import System.Random (randomRIO)
import Text.Read (readMaybe)

-- Función principal que inicia el juego
main :: IO ()
main = do
    hSetBuffering stdout LineBuffering  -- Configura el modo de buffering de salida
    juegoInicial <- generarNivel 1 3  -- Genera el nivel inicial con 3 vidas
    loop juegoInicial  -- Inicia el bucle del juego

-- Genera un nuevo nivel con posiciones aleatorias para el coche y la meta
generarNivel :: Int -> Int -> IO Juego
generarNivel nNivel nVidas = do
    inicioX <- randomRIO (3, 5)   :: IO Int  -- Genera posición X inicial aleatoria
    inicioY <- randomRIO (3, 18)  :: IO Int  -- Genera posición Y inicial aleatoria
    metaX   <- randomRIO (28, 30) :: IO Int  -- Genera posición X de la meta aleatoria
    metaY   <- randomRIO (3, 18)  :: IO Int  -- Genera posición Y de la meta aleatoria

    let inicio = (inicioX, inicioY)  -- Crea la coordenada inicial
    let meta = (metaX, metaY)  -- Crea la coordenada de la meta

    let numObstaculos = if nNivel == 1 then 0 else (nNivel - 1) * 3  -- Calcula el número de obstáculos

    obs <- generarObstaculos numObstaculos inicio meta []  -- Genera las posiciones de los obstáculos

    return $ crearJuego inicio meta obs nVidas nNivel  -- Crea y devuelve el juego

-- Genera una lista de obstáculos aleatorios
generarObstaculos :: Int -> Coord -> Coord -> [Coord] -> IO [Coord]
generarObstaculos 0 _ _ acc = return acc  -- Si no quedan obstáculos por generar, devuelve la lista acumulada
generarObstaculos n start end acc = do
    rx <- randomRIO (8, 25) :: IO Int  -- Genera posición X aleatoria para el obstáculo
    ry <- randomRIO (3, 18) :: IO Int  -- Genera posición Y aleatoria para el obstáculo
    let pos = (rx, ry)  -- Crea la coordenada del obstáculo
    if pos == start || pos == end || pos `elem` acc  -- Verifica si la posición es válida
        then generarObstaculos n start end acc  -- Si no es válida, intenta generar otra
        else generarObstaculos (n - 1) start end (pos : acc)  -- Agrega la posición y continúa

-- Bucle principal del juego que maneja la entrada del usuario
loop :: Juego -> IO ()
loop estadoActual = do
    linea <- getLine  -- Lee la línea de entrada del usuario
    let partes = splitOn " " linea  -- Divide la línea en partes

    case partes of
        ["RESET_PATH"] -> do
            let nuevoEstado = limpiarCamino estadoActual  -- Limpia el camino dibujado
            responder nuevoEstado  -- Responde con el nuevo estado
            loop nuevoEstado  -- Continúa el bucle

        ["DIBUJAR", xStr, yStr] -> do
            case (readMaybe xStr, readMaybe yStr) of
                (Just x, Just y) -> do
                    let nuevoEstado = agregarDibujo (x, y) estadoActual  -- Agrega un nuevo punto al camino
                    responder nuevoEstado  -- Responde con el nuevo estado
                    loop nuevoEstado  -- Continúa el bucle
                _ -> loop estadoActual  -- Si la entrada no es válida, continúa

        ["INICIAR"] -> do
            let nuevoEstado = intentarIniciar estadoActual  -- Intenta iniciar el juego
            responder nuevoEstado  -- Responde con el nuevo estado
            loop nuevoEstado  -- Continúa el bucle

        ["TICK"] -> do
            let nuevoEstado = avanzarCoche estadoActual  -- Avanza el coche en el juego
            responder nuevoEstado  -- Responde con el nuevo estado
            loop nuevoEstado  -- Continúa el bucle

        ["SIGUIENTE_NIVEL"] -> do
            nuevoJuego <- generarNivel (nivelActual (estadoActual) + 1) (vidas estadoActual)  -- Genera el siguiente nivel
            responder nuevoJuego  -- Responde con el nuevo estado
            loop nuevoJuego  -- Continúa el bucle

        ["REINTENTAR_O_PERDER"] -> do
            let vidasRestantes = vidas estadoActual - 1  -- Reduce el número de vidas
            if vidasRestantes > 0
                then do
                    nuevoJuego <- generarNivel (nivelActual estadoActual) vidasRestantes  -- Genera un nuevo nivel con vidas restantes
                    responder nuevoJuego  -- Responde con el nuevo estado
                    loop nuevoJuego  -- Continúa el bucle
                else do
                    let juegoGameOver = estadoActual { estado = GameOver, vidas = 0 }  -- Cambia el estado a Game Over
                    responder juegoGameOver  -- Responde con el nuevo estado
                    loop juegoGameOver  -- Continúa el bucle

        ["REINICIAR_COMPLETO"] -> do
            juegoNuevo <- generarNivel 1 3  -- Reinicia el juego a nivel 1 con 3 vidas
            responder juegoNuevo  -- Responde con el nuevo estado
            loop juegoNuevo  -- Continúa el bucle

        ["QUIT"] -> return ()  -- Sale del juego
        _ -> loop estadoActual  -- Si el comando no es reconocido, continúa

-- Responde al cliente con el estado actual del juego en formato JSON
responder :: Juego -> IO ()
responder st = do
    LBS.putStrLn $ encode st  -- Codifica el estado del juego a JSON y lo imprime
    hFlush stdout  -- Asegura que la salida se muestre inmediatamente