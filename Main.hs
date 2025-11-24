module Main where

import Juego
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as LBS (putStrLn)
import System.IO (hFlush, stdout, hSetBuffering, BufferMode(..))
import Control.Monad (forever)
import Data.List.Split (splitOn)
import System.Random (randomRIO)
import Text.Read (readMaybe)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    -- Ajusta rangos si quieres
    inicioX <- randomRIO (2, 5)   :: IO Int
    inicioY <- randomRIO (2, 20)  :: IO Int
    metaX   <- randomRIO (25, 30) :: IO Int
    metaY   <- randomRIO (2, 20)  :: IO Int

    let juegoInicial = crearJuego (inicioX, inicioY) (metaX, metaY)
    loop juegoInicial

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

        ["QUIT"] -> return ()
        _ -> loop estadoActual

responder :: Juego -> IO ()
responder st = do
    LBS.putStrLn $ encode st
    hFlush stdout