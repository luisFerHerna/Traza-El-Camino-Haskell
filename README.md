# Traza el Camino 🚗💨

Un juego de estrategia y dibujo donde controlas el destino de un coche utilizando lógica funcional y una interfaz gráfica interactiva.

**Tecnologías:** Haskell (Lógica Backend) + Python/Pygame (Frontend Visual).

## 📋 Requisitos
* [GHC & Cabal](https://www.haskell.org/ghcup/) (Para compilar Haskell)
* [Python 3](https://www.python.org/) con Pygame (\`pip install pygame\`)

## 🚀 Instalación y Ejecución

1.  **Clonar el repositorio:**
    \`\`\`bash
    git clone https://github.com/luisFerHerna/Traza-El-Camino-Haskell.git
    cd Traza-El-Camino-Haskell
    \`\`\`

2.  **Compilar el Backend (Haskell):**
    \`\`\`bash
    cabal install --installdir=. --overwrite-policy=always
    \`\`\`

3.  **Ejecutar el Cliente (Python):**
    \`\`\`bash
    python juego_cliente.py
    \`\`\`

## 🎮 Cómo Jugar
1.  **Objetivo:** Llevar el coche hasta la casa 🏠.
2.  **Controles:**
    * **Click en el coche y arrastra** para dibujar el camino.
    * **Suelta el click** para iniciar el movimiento.
    * **[R]**: Reiniciar nivel / Reintentar.
3.  **Mecánicas:**
    * Si dibujas fuera del mapa o el camino se corta, el coche caerá al vacío.
    * El nivel se genera aleatoriamente en cada partida.

## 📁 Estructura
* \`Juego.hs\`: Lógica pura, estados y algoritmo de Bresenham.
* \`Main.hs\`: Servidor de entrada/salida y aleatoriedad.
* \`juego_cliente.py\`: Interfaz gráfica, animaciones y manejo de eventos.
