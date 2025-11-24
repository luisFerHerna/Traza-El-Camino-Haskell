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
-- Coord: par de enteros que representa una celda del "mapa" (x, y).
type Coord = (Int, Int)

-- Elemento: cada celda del mapa puede ser Carretera, Obstaculo, Meta o Vacio.
-- Se deriva ToJSON para poder serializar el nivel a JSON cuando se comunique con el cliente.
data Elemento = Carretera | Obstaculo | Meta | Vacio
    deriving (Eq, Show, Generic, ToJSON)

-- EstadoJuego: estados lógicos que describe la máquina de estados del juego.
-- - Dibujando: el jugador está trazando el camino.
-- - EnCurso: el coche se mueve siguiendo el camino.
-- - Caido: el coche se salió del camino (falló).
-- - Chocado: reservado para colisiones (no usado activamente aquí).
-- - Ganado: el coche llegó a la meta.
data EstadoJuego = Dibujando | EnCurso | Caido | Chocado | Ganado
    deriving (Eq, Show, Generic, ToJSON)

-- Nivel: mapa que asocia una coordenada con el Elemento que contiene.
type Nivel = Map.Map Coord Elemento

-- Juego: estado completo del juego que se serializa para el cliente.
-- - cochePos: posición actual del coche en coordenadas de celda.
-- - metaPos: posición objetivo/meta.
-- - nivel: mapa de elementos (uso limitado en esta versión).
-- - estado: estado lógico actual (EstadoJuego).
-- - caminoDibujado: lista de coordenadas que representan el camino trazado por el jugador.
-- - angulo: orientación visual del coche en grados (usada por el cliente para rotar sprite).
data Juego = Juego {
    cochePos :: Coord,
    metaPos :: Coord,
    nivel :: Nivel,
    estado :: EstadoJuego,
    caminoDibujado :: [Coord],
    angulo :: Float
} deriving (Show, Generic)

-- Implementación de ToJSON personalizada para controlar exactamente qué se envía al cliente.
-- Convertimos el mapa 'nivel' a una lista (Map.toList) para que sea serializable fácilmente.
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

-- crearJuego: construye un estado inicial mínimo.
-- - inicio: coordenada inicial del coche.
-- - meta: coordenada de la meta.
-- El nivel se inicializa marcando la meta; estado empieza en Dibujando y el camino contiene el inicio.
crearJuego :: Coord -> Coord -> Juego
crearJuego inicio meta = Juego {
    cochePos = inicio,
    metaPos = meta,
    nivel = Map.fromList [(meta, Meta)],
    estado = Dibujando,
    caminoDibujado = [inicio],
    angulo = 0.0
}

-- limpiarCamino: reinicia el camino dibujado y vuelve al estado Dibujando.
-- Útil cuando el cliente solicita resetear el path para trazar desde el coche.
limpiarCamino :: Juego -> Juego
limpiarCamino juego = juego {
    estado = Dibujando,
    caminoDibujado = [],
    angulo = 0.0
}

-- intentarIniciar: si hay al menos un punto en caminoDibujado, pasa a EnCurso.
-- Si no hay camino, se queda como estaba (no inicia).
intentarIniciar :: Juego -> Juego
intentarIniciar juego
    | length (caminoDibujado juego) > 0 = juego { estado = EnCurso }
    | otherwise = juego

-- calcularAngulo: obtiene la orientación en grados entre dos celdas.
-- Se usa para dar una dirección visual al sprite del coche.
calcularAngulo :: Coord -> Coord -> Float
calcularAngulo (x1, y1) (x2, y2) =
    let dx = fromIntegral (x2 - x1)
        dy = fromIntegral (y2 - y1)
        rados = atan2 dy dx
    in rados * (180.0 / pi)

-- esAdyacente: verifica si dos celdas están adyacentes (incluye diagonales).
-- Evita que el coche "salte" a una posición no contigua del camino.
esAdyacente :: Coord -> Coord -> Bool
esAdyacente (x1, y1) (x2, y2) = abs (x1 - x2) <= 1 && abs (y1 - y2) <= 1

-- avanzarCoche: mueve el coche un paso siguiendo el primer elemento de caminoDibujado.
-- Casos importantes:
-- - Si el camino está vacío:
--     * Si el coche está en la meta -> Ganado
--     * Si no -> Caido (se acabó el camino antes de llegar)
-- - Si hay un siguiente punto y no es adyacente -> Caido (se considera salto)
-- - Si el siguiente punto es la meta -> actualizar posición y marcar Ganado
-- - En caso normal -> actualizar cochePos, consumir el primer punto del camino y actualizar angulo.
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

-- Caso por defecto: si el juego no está en EnCurso, no se modifica.
avanzarCoche juego = juego

-- ALGORITMO DE BRESENHAM (Para trazar líneas)
-- bresenham: dado un origen y un destino devuelve la lista de celdas que atraviesa la línea.
-- Se usa para rellenar los puntos intermedios cuando el usuario arrastra el ratón.
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

-- agregarDibujo: añade un segmento de trazado desde la última posición del camino hasta la
-- coordenada proporcionada usando bresenham para interpolar puntos.
-- Comportamiento:
-- - Solo modifica el juego cuando el estado es Dibujando.
-- - Evita duplicar el punto de origen usando 'tail' de la lista generada por bresenham.
agregarDibujo :: Coord -> Juego -> Juego
agregarDibujo coordDestino juego@(Juego {estado = Dibujando, caminoDibujado = camino})
    | null camino = juego { caminoDibujado = [coordDestino] }
    | otherwise =
        let coordOrigen = last camino
            nuevosPuntos = bresenham coordOrigen coordDestino
            -- 'tail' evita duplicar el punto de origen
            puntosAAnadir = if null nuevosPuntos then [] else tail nuevosPuntos
        in juego { caminoDibujado = camino ++ puntosAAnadir }

-- Si no estamos en Dibujando, no se añade nada.
agregarDibujo _ juego = juego