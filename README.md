# 🚗 Lleva al Coche a Casa (Haskell + Python/Pygame)

![Estado del Proyecto](https://img.shields.io/badge/Estado-Finalizado-Verde)
![Haskell](https://img.shields.io/badge/Backend-Haskell-purple)
![Python](https://img.shields.io/badge/Frontend-Python%20%7C%20Pygame-blue)

Un juego de puzzle interactivo donde el objetivo es guiar un coche hasta su meta dibujando el camino manualmente. Este proyecto destaca por su **arquitectura híbrida**, utilizando **Haskell** para la lógica robusta del juego y el cálculo de rutas, y **Python (Pygame)** para la interfaz gráfica y el manejo de eventos.

## 📋 Características

* **Arquitectura Híbrida:** Comunicación mediante tuberías (pipes) estándar (`stdin`/`stdout`) e intercambio de mensajes en formato JSON.
* **Lógica Funcional:** El estado del juego, detección de colisiones y validación de rutas están implementados puramente en Haskell via `Juego.hs`.
* **Algoritmo de Bresenham:** Implementación personalizada en Haskell para calcular los puntos discretos del camino dibujado por el usuario.
* **Sistema de Niveles:** Generación procedimental de obstáculos y posiciones (Coche/Meta) con dificultad incremental.
* **Gestión de Vidas:** Sistema de intentos y "Game Over".
* **Assets y Audio:** Soporte para sprites, imágenes personalizadas y efectos de sonido (con fallbacks geométricos si no se encuentran los archivos).

## 🛠️ Requisitos Previos

Para ejecutar este proyecto necesitarás tener instalado:

### Haskell (Backend)
* **GHC** (Glasgow Haskell Compiler)
* Paquetes necesarios (instalables vía cabal o stack):
    * `aeson` (para manejo de JSON)
    * `random` (para generación aleatoria)
    * `split` (para procesamiento de cadenas)

### Python (Frontend)
* **Python 3.x**
* **Pygame**: `pip install pygame`

## 🚀 Instalación y Ejecución

Sigue estos pasos para compilar y correr el juego:

1.  **Clonar el repositorio:**
    ```bash
    git clone [https://github.com/tu-usuario/nombre-repo.git](https://github.com/tu-usuario/nombre-repo.git)
    cd nombre-repo
    ```

2.  **Compilar el Backend (Haskell):**
    Es necesario compilar el código Haskell para crear el ejecutable que Python llamará.
    ```bash
    ghc --make Main.hs -o juego_haskell
    ```
    *Nota: Asegúrate de que el ejecutable resultante se llame `juego_haskell` (o `juego_haskell.exe` en Windows), ya que el script de Python lo busca con ese nombre.*

3.  **Ejecutar el Frontend (Python):**
    ```bash
    python juego_cliente.py
    ```

## 🎮 Controles

* **Click Izquierdo + Arrastrar:** Dibujar el camino (mientras el estado sea "Dibujando").
* **Botón INICIAR:** Arranca el coche para que siga la ruta dibujada.
* **Teclas (según implementación):**
    * `R`: Reiniciar juego completo (en pantalla de Game Over).
    * `Espacio`: Avanzar al siguiente nivel o reintentar tras chocar.

## 📂 Estructura del Proyecto

* `Main.hs`: Punto de entrada del backend. Maneja el bucle de IO, recibe comandos de Python y responde con el estado en JSON.
* `Juego.hs`: Módulo con la lógica pura del juego. Define los tipos de datos (`Juego`, `EstadoJuego`), funciones de movimiento y el algoritmo de trazado de líneas.
* `juego_cliente.py`: Interfaz gráfica en Pygame. Gestiona los sprites, inputs del mouse y lanza el subproceso de Haskell.
* **Recursos (Opcionales):**
    * `frames.png`, `casa.png`, `obstaculo.png`: Imágenes del juego.
    * `*.mp3`: Archivos de audio para efectos y música.

## 🐛 Solución de Problemas Comunes

* **Error: `juego_haskell not found`**: Asegúrate de haber compilado el archivo Haskell y que el ejecutable está en la misma carpeta que el script de Python.
* **Errores de JSON**: Si modificas el código Haskell, asegúrate de que la instancia `ToJSON` coincida con lo que espera el script de Python.

## ✒️ Autor

Proyecto desarrollado como demostración de integración entre programación funcional (Haskell) y programación imperativa/gráfica (Python).
