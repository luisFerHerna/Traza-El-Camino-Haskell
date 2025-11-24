{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Juego (
    Coord, Elemento(..), Nivel, EstadoJuego(..), Juego(..),
    crearJuego, avanzarCoche, agregarDibujo, intentarIniciar, limpiarCamino
) where

import qualified Data.Map as Map
import Data.Aeson
import GHC.Generics

-- TIPOS DE DATOS
type Coord = (Int, Int)

data Elemento = Carretera | Obstaculo | Meta | Vacio
    deriving (Eq, Show, Generic, ToJSON)

data EstadoJuego = Dibujando | EnCurso | Caido | Chocado | Ganado
    deriving (Eq, Show, Generic, ToJSON)

type Nivel = Map.Map Coord Elemento

data Juego = Juego {
    cochePos :: Coord,
    metaPos :: Coord,
    nivel :: Nivel,
    estado :: EstadoJuego,
    caminoDibujado :: [Coord],
    angulo :: Float
} deriving (Show, Generic)

instance ToJSON Juego where
    toJSON juego = object
        [ "cochePos"       .= cochePos juego
        , "metaPos"        .= metaPos juego
        , "estado"         .= estado juego
        , "caminoDibujado" .= caminoDibujado juego
        , "angulo"         .= angulo juego
        , "nivel"          .= Map.toList (nivel juego)
        ]

-- LÓGICA DEL JUEGO

crearJuego :: Coord -> Coord -> Juego
crearJuego inicio meta = Juego {
    cochePos = inicio,
    metaPos = meta,
    nivel = Map.fromList [(meta, Meta)],
    estado = Dibujando,
    caminoDibujado = [inicio],
    angulo = 0.0
}

-- Reinicia el camino a VACIO para obligar a dibujar desde el coche
limpiarCamino :: Juego -> Juego
limpiarCamino juego = juego {
    estado = Dibujando,
    caminoDibujado = [],
    angulo = 0.0
}

intentarIniciar :: Juego -> Juego
intentarIniciar juego
    | length (caminoDibujado juego) > 0 = juego { estado = EnCurso }
    | otherwise = juego

calcularAngulo :: Coord -> Coord -> Float
calcularAngulo (x1, y1) (x2, y2) =
    let dx = fromIntegral (x2 - x1)
        dy = fromIntegral (y2 - y1)
        rados = atan2 dy dx
    in rados * (180.0 / pi)

-- Verifica si dos puntos están pegados (evita saltos/teletransportación)
esAdyacente :: Coord -> Coord -> Bool
esAdyacente (x1, y1) (x2, y2) = abs (x1 - x2) <= 1 && abs (y1 - y2) <= 1

avanzarCoche :: Juego -> Juego
avanzarCoche juego@(Juego {estado = EnCurso, caminoDibujado = camino, metaPos = meta, cochePos = actual}) =
    case camino of
        [] ->
            if actual == meta
            then juego { estado = Ganado }
            else juego { estado = Caido } -- Se acabó el camino y no llegó

        (siguientePos : restoCamino) ->
            -- Si el siguiente punto NO es adyacente, el coche cae
            if not (esAdyacente actual siguientePos) && actual /= siguientePos
            then juego { estado = Caido }
            else
                let nuevoAngulo = calcularAngulo actual siguientePos
                in if siguientePos == meta
                   then juego { cochePos = siguientePos, caminoDibujado = [], estado = Ganado, angulo = nuevoAngulo }
                   else juego { cochePos = siguientePos, caminoDibujado = restoCamino, angulo = nuevoAngulo }

avanzarCoche juego = juego

-- ALGORITMO DE BRESENHAM (Para trazar líneas)
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
        | x == x1 && y == y1 = []
        | otherwise =
            let e2 = 2 * e
                (nextX, e_x) = if e2 > -dy' then (x + sx', e - dy') else (x, e)
                (nextY, e_y) = if e2 < dx' then (y + sy', e_x + dx') else (y, e_x)
            in (nextX, nextY) : paso nextX nextY e_y dx' dy' sx' sy'

agregarDibujo :: Coord -> Juego -> Juego
agregarDibujo coordDestino juego@(Juego {estado = Dibujando, caminoDibujado = camino})
    | null camino = juego { caminoDibujado = [coordDestino] }
    | otherwise =
        let coordOrigen = last camino
            nuevosPuntos = bresenham coordOrigen coordDestino
            -- 'tail' evita duplicar el punto de origen
            puntosAAnadir = if null nuevosPuntos then [] else tail nuevosPuntos
        in juego { caminoDibujado = camino ++ puntosAAnadir }

agregarDibujo _ juego = juego