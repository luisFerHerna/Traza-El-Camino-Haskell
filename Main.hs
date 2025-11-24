module Main where

-- Importar el módulo que contiene la lógica del juego (Juego.hs)
import Juego

-- Para serializar el estado del juego a JSON y enviarlo al cliente
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as LBS (putStrLn)

-- Control de buffering y flushing de stdout (importante para comunicación por pipes)
import System.IO (hFlush, stdout, hSetBuffering, BufferMode(..))

-- Utilidades: bucles, dividir cadena, números aleatorios y parseo seguro
import Control.Monad (forever)
import Data.List.Split (splitOn)
import System.Random (randomRIO)
import Text.Read (readMaybe)

-- Punto de entrada del programa Haskell que actúa como "servidor" lógico.
-- Este ejecutable espera comandos por stdin y responde por stdout con JSON.
main :: IO ()
main = do
    -- Establecer buffering de stdout en LineBuffering para que cada línea se envíe inmediatamente.
    hSetBuffering stdout LineBuffering

    -- Generar posiciones aleatorias iniciales para el coche y la meta.
    -- Los rangos se pueden ajustar si se desea cambiar tamaño/posición del mapa.
    inicioX <- randomRIO (2, 5)   :: IO Int
    inicioY <- randomRIO (2, 20)  :: IO Int
    metaX   <- randomRIO (25, 30) :: IO Int
    metaY   <- randomRIO (2, 20)  :: IO Int

    -- Crear el estado inicial del juego usando crearJuego (definido en Juego.hs)
    let juegoInicial = crearJuego (inicioX, inicioY) (metaX, metaY)

    -- Entrar en el bucle principal con el estado inicial
    loop juegoInicial

-- loop: bucle principal que lee líneas desde stdin, interpreta comandos y
-- actualiza/contesta con el nuevo estado del juego.
loop :: Juego -> IO ()
loop estadoActual = do
    -- Leer una línea (comando) desde stdin enviada por el cliente (ej. el cliente Python)
    linea <- getLine

    -- Dividir la línea en palabras para interpretar el comando y sus argumentos
    let partes = splitOn " " linea

    -- Patrones de comandos aceptados:
    -- RESET_PATH    -> reinicia el camino dibujado en el estado lógico
    -- DIBUJAR x y   -> añade puntos al camino usando las coordenadas de celda
    -- INICIAR       -> intenta cambiar el estado a EnCurso para que el coche avance
    -- TICK          -> avanzar un paso la lógica del coche (avanzarCoche)
    -- QUIT          -> terminar el programa
    case partes of
        ["RESET_PATH"] -> do
            -- Limpiar el camino y responder con el nuevo estado
            let nuevoEstado = limpiarCamino estadoActual
            responder nuevoEstado
            loop nuevoEstado

        ["DIBUJAR", xStr, yStr] -> do
            -- Intentar parsear las coordenadas recibidas (cadena -> Int de forma segura)
            case (readMaybe xStr, readMaybe yStr) of
                (Just x, Just y) -> do
                    -- Añadir el dibujo (se usa Bresenham en Juego.agregarDibujo)
                    let nuevoEstado = agregarDibujo (x, y) estadoActual
                    responder nuevoEstado
                    loop nuevoEstado
                -- Si no se pudieron parsear los enteros, ignorar y continuar con el mismo estado
                _ -> loop estadoActual

        ["INICIAR"] -> do
            -- Intentar iniciar el movimiento del coche (si hay camino dibujado)
            let nuevoEstado = intentarIniciar estadoActual
            responder nuevoEstado
            loop nuevoEstado

        ["TICK"] -> do
            -- Avanzar la lógica del coche un paso y devolver el estado actualizado
            let nuevoEstado = avanzarCoche estadoActual
            responder nuevoEstado
            loop nuevoEstado

        ["QUIT"] -> return () -- Salir del bucle y terminar el programa

        -- Cualquier otra entrada se ignora (silenciosa) y se continúa con el estado actual
        _ -> loop estadoActual

-- responder: serializa el estado de tipo Juego a JSON y lo envía por stdout seguido de flush.
-- El cliente (Python) espera recibir exactamente una línea JSON por cada comando relevante.
responder :: Juego -> IO ()
responder st = do
    -- encode produce un ByteString Lazy con la representación JSON
    LBS.putStrLn $ encode st
    -- Asegura que el contenido llegue inmediatamente por stdout (útil cuando se usa con pipes)
    hFlush stdout