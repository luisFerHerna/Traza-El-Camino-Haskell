{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

-- Módulo principal del juego
module Juego (
    Coord, Elemento(..), Nivel, EstadoJuego(..), Juego(..),
    crearJuego, avanzarCoche, agregarDibujo, intentarIniciar, limpiarCamino
) where

import qualified Data.Map as Map
import Data.Aeson
import GHC.Generics

-- Tipo de coordenadas en el juego
type Coord = (Int, Int)

-- Definición de los elementos que pueden aparecer en el nivel
data Elemento = Carretera | Meta | Vacio | ObstaculoItem
    deriving (Eq, Show, Generic, ToJSON)

-- Estados posibles del juego
data EstadoJuego = Dibujando | EnCurso | Caido | Ganado | Chocado | GameOver
    deriving (Eq, Show, Generic, ToJSON)

-- Mapa que representa el nivel del juego
type Nivel = Map.Map Coord Elemento

-- Estructura que representa el estado del juego
data Juego = Juego {
    cochePos :: Coord,        -- Posición actual del coche
    metaPos :: Coord,         -- Posición de la meta
    obstaculos :: [Coord],    -- Lista de posiciones de obstáculos
    nivel :: Nivel,           -- Mapa del nivel
    estado :: EstadoJuego,    -- Estado actual del juego
    caminoDibujado :: [Coord],-- Lista de coordenadas del camino dibujado
    angulo :: Float,          -- Ángulo de dirección del coche
    vidas :: Int,             -- Número de vidas del jugador
    nivelActual :: Int        -- Nivel actual del juego
} deriving (Show, Generic)

-- Conversión de la estructura Juego a formato JSON
instance ToJSON Juego where
    toJSON juego = object
        [ "cochePos"       .= cochePos juego
        , "metaPos"        .= metaPos juego
        , "obstaculos"     .= obstaculos juego
        , "estado"         .= estado juego
        , "caminoDibujado" .= caminoDibujado juego
        , "angulo"         .= angulo juego
        , "vidas"          .= vidas juego
        , "nivelActual"    .= nivelActual juego
        , "nivel"          .= Map.toList (nivel juego)
        ]

-- Función para crear un nuevo juego
crearJuego :: Coord -> Coord -> [Coord] -> Int -> Int -> Juego
crearJuego inicio meta obs nVidas nNivel =
    let mapaBase = Map.fromList ([(meta, Meta)] ++ [(o, ObstaculoItem) | o <- obs])
    in Juego {
        cochePos = inicio,
        metaPos = meta,
        obstaculos = obs,
        nivel = mapaBase,
        estado = Dibujando,
        caminoDibujado = [inicio],
        angulo = 0.0,
        vidas = nVidas,
        nivelActual = nNivel
    }

-- Función para limpiar el camino dibujado y reiniciar el estado
limpiarCamino :: Juego -> Juego
limpiarCamino juego = juego {
    estado = Dibujando,
    caminoDibujado = [],
    angulo = 0.0
}

-- Función para intentar iniciar el juego
intentarIniciar :: Juego -> Juego
intentarIniciar juego
    | length (caminoDibujado juego) > 0 = juego { estado = EnCurso }  -- Cambia el estado a EnCurso si hay camino dibujado
    | otherwise = juego  -- Mantiene el estado actual

-- Calcula el ángulo entre dos coordenadas
calcularAngulo :: Coord -> Coord -> Float
calcularAngulo (x1, y1) (x2, y2) =
    let dx = fromIntegral (x2 - x1)
        dy = fromIntegral (y2 - y1)
        rados = atan2 dy dx  -- Calcula el ángulo en radianes
    in rados * (180.0 / pi)  -- Convierte a grados

-- Verifica si dos coordenadas son adyacentes
esAdyacente :: Coord -> Coord -> Bool
esAdyacente (x1, y1) (x2, y2) = abs (x1 - x2) <= 1 && abs (y1 - y2) <= 1

-- Verifica si hay obstáculos cercanos a una coordenada
obstaculosProximos :: Coord -> [Coord] -> Bool
obstaculosProximos (x, y) obs =
    let rango = [-2, -1, 0, 1, 2]  -- Rango de búsqueda
        vecinos = [(x + dx, y + dy) | dx <- rango, dy <- rango]  -- Genera las coordenadas vecinas
    in any (`elem` obs) vecinos  -- Verifica si hay obstáculos en las vecinas

-- Verifica si el coche ha llegado a la meta
llegoAMeta :: Coord -> Coord -> Bool
llegoAMeta (x, y) (mx, my) = abs (x - mx) <= 1 && abs (y - my) <= 1

-- Función para avanzar el coche en el juego
avanzarCoche :: Juego -> Juego
avanzarCoche juego@(Juego {estado = EnCurso, caminoDibujado = camino, metaPos = meta, cochePos = actual, obstaculos = obs}) =
    case camino of
        [] ->  -- Si no hay más camino
            if llegoAMeta actual meta
            then juego { estado = Ganado }  -- Si llegó a la meta, cambia el estado a Ganado
            else juego { estado = Caido }  -- Si no, cambia el estado a Caido

        (siguientePos : restoCamino) ->  -- Si hay más posiciones en el camino
            if not (llegoAMeta siguientePos meta) && (siguientePos `elem` obs || obstaculosProximos siguientePos obs)
            then juego { estado = Chocado, cochePos = siguientePos }  -- Si choca con un obstáculo
            else if not (esAdyacente actual siguientePos) && actual /= siguientePos
            then juego { estado = Caido }  -- Si no se mueve a una posición adyacente, cae
            else
                let nuevoAngulo = calcularAngulo actual siguientePos  -- Calcula el nuevo ángulo
                in if llegoAMeta siguientePos meta
                   then juego { cochePos = siguientePos, caminoDibujado = [], estado = Ganado, angulo = nuevoAngulo }  -- Si llega a la meta
                   else juego { cochePos = siguientePos, caminoDibujado = restoCamino, angulo = nuevoAngulo }  -- Actualiza la posición y el camino

-- Si el estado no es EnCurso, no se hace nada
avanzarCoche juego = juego

-- Algoritmo de Bresenham para dibujar líneas entre dos coordenadas
bresenham :: Coord -> Coord -> [Coord]
bresenham (x0, y0) (x1, y1) =
    let dx = abs (x1 - x0)
        dy = abs (y1 - y0)
        sx = signum (x1 - x0)
        sy = signum (y1 - y0)
        err = dx - dy
    in (x0, y0) : paso x0 y0 err dx dy sx sy
  where
    paso x y e dx' dy' sx' sy'
        | x == x1 && y == y1 = []  -- Si se llega al final, termina
        | otherwise =
            let e2 = 2 * e
                (nextX, e_x) = if e2 > -dy' then (x + sx', e - dy') else (x, e)
                (nextY, e_y) = if e2 < dx' then (y + sy', e_x + dx') else (y, e_x)
            in (nextX, nextY) : paso nextX nextY e_y dx' dy' sx' sy'

-- Agrega un dibujo al camino del juego
agregarDibujo :: Coord -> Juego -> Juego
agregarDibujo coordDestino juego@(Juego {estado = Dibujando, caminoDibujado = camino})
    | null camino = juego { caminoDibujado = [coordDestino] }  -- Si no hay camino, inicia uno nuevo
    | otherwise =
        let coordOrigen = last camino  -- Obtiene la última posición del camino
            nuevosPuntos = bresenham coordOrigen coordDestino  -- Calcula los puntos intermedios
            puntosAAnadir = if null nuevosPuntos then [] else tail nuevosPuntos  -- Excluye el origen
        in juego { caminoDibujado = camino ++ puntosAAnadir }  -- Agrega los nuevos puntos al camino
agregarDibujo _ juego = juego  -- Si el estado no es Dibujando, no se hace nada