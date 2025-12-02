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

type Coord = (Int, Int)

data Elemento = Carretera | Meta | Vacio | ObstaculoItem
    deriving (Eq, Show, Generic, ToJSON)

data EstadoJuego = Dibujando | EnCurso | Caido | Ganado | Chocado | GameOver
    deriving (Eq, Show, Generic, ToJSON)

type Nivel = Map.Map Coord Elemento

data Juego = Juego {
    cochePos :: Coord,
    metaPos :: Coord,
    obstaculos :: [Coord],
    nivel :: Nivel,
    estado :: EstadoJuego,
    caminoDibujado :: [Coord],
    angulo :: Float,
    vidas :: Int,
    nivelActual :: Int
} deriving (Show, Generic)

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

esAdyacente :: Coord -> Coord -> Bool
esAdyacente (x1, y1) (x2, y2) = abs (x1 - x2) <= 1 && abs (y1 - y2) <= 1

obstaculosProximos :: Coord -> [Coord] -> Bool
obstaculosProximos (x, y) obs =
    let rango = [-2, -1, 0, 1, 2]
        vecinos = [(x + dx, y + dy) | dx <- rango, dy <- rango]
    in any (`elem` obs) vecinos

llegoAMeta :: Coord -> Coord -> Bool
llegoAMeta (x, y) (mx, my) = abs (x - mx) <= 1 && abs (y - my) <= 1

avanzarCoche :: Juego -> Juego
avanzarCoche juego@(Juego {estado = EnCurso, caminoDibujado = camino, metaPos = meta, cochePos = actual, obstaculos = obs}) =
    case camino of
        [] ->
            if llegoAMeta actual meta
            then juego { estado = Ganado }
            else juego { estado = Caido }

        (siguientePos : restoCamino) ->
            if not (llegoAMeta siguientePos meta) && (siguientePos `elem` obs || obstaculosProximos siguientePos obs)
            then juego { estado = Chocado, cochePos = siguientePos }
            else if not (esAdyacente actual siguientePos) && actual /= siguientePos
            then juego { estado = Caido }
            else
                let nuevoAngulo = calcularAngulo actual siguientePos
                in if llegoAMeta siguientePos meta
                   then juego { cochePos = siguientePos, caminoDibujado = [], estado = Ganado, angulo = nuevoAngulo }
                   else juego { cochePos = siguientePos, caminoDibujado = restoCamino, angulo = nuevoAngulo }

avanzarCoche juego = juego

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
            puntosAAnadir = if null nuevosPuntos then [] else tail nuevosPuntos
        in juego { caminoDibujado = camino ++ puntosAAnadir }
agregarDibujo _ juego = juego