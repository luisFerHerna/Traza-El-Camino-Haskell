import subprocess
import pygame
import json
import sys
import os

# GitHub Copilot
# Archivo cliente en Python que actúa como interfaz gráfica del juego.
# Se comunica con el ejecutable Haskell (proceso hijo) mediante stdin/stdout
# para recibir el estado lógico del juego en JSON y enviar comandos (TICK, DIBUJAR, etc).

HASKELL_EXECUTABLE = './juego_haskell'  # Ejecutable Haskell que contiene la lógica del juego
ANCHO_VENTANA, ALTO_VENTANA = 700, 500   # Tamaño de la ventana Pygame
TAMANO_CELDA = 20                        # Tamaño en píxeles de cada celda del "mapa" lógico
DURACION_FRAME = 80                      # Milisegundos entre frames de animación lógica
VELOCIDAD_CAIDA = 8                      # Velocidad en píxeles por frame para la animación de caída

pygame.init()
pantalla = pygame.display.set_mode((ANCHO_VENTANA, ALTO_VENTANA))
pygame.display.set_caption("Traza el camino - Versión Final")
reloj = pygame.time.Clock()  # Reloj para controlar FPS y medir dt

# Recursos gráficos (sprites)
sprites_coche = []    # Lista de frames para el coche (si existe frames.png)
imagen_casa = None    # Imagen de la meta/casa (si existe casa.png)

try:
    # Intentar cargar hoja de sprites para el coche (4 frames en horizontal)
    if os.path.exists('frames.png'):
        hoja = pygame.image.load('frames.png').convert_alpha()
        ancho_frame = hoja.get_width() // 4
        alto_hoja = hoja.get_height()
        for i in range(4):
            rect = pygame.Rect(i * ancho_frame, 0, ancho_frame, alto_hoja)
            frame = hoja.subsurface(rect)
            # Escalar a un tamaño relativo a la celda para que se vea bien
            frame = pygame.transform.scale(frame, (TAMANO_CELDA + 12, TAMANO_CELDA + 12))
            sprites_coche.append(frame)

    # Intentar cargar imagen de la casa/meta
    if os.path.exists('casa.png'):
        img_casa = pygame.image.load('casa.png').convert_alpha()
        imagen_casa = pygame.transform.scale(img_casa, (TAMANO_CELDA + 15, TAMANO_CELDA + 15))

except Exception as e:
    # Si falla cargar imágenes, no es crítico: se usan dibujos simples en su lugar
    print(f"Advertencia cargando imágenes: {e}")


def dibujar_juego(estado_juego, frame_actual, offset_caida_y, juego_terminado_visualmente):
    """
    Dibuja en pantalla el estado recibido desde Haskell.
    - estado_juego: diccionario con claves como 'caminoDibujado', 'metaPos', 'cochePos', 'angulo', 'estado'
    - frame_actual: índice de animación (para elegir sprite)
    - offset_caida_y: desplazamiento vertical aplicado al coche durante animación de caída
    - juego_terminado_visualmente: booleano que indica si la animación finalizó
    """
    pantalla.fill((255, 255, 255))  # Fondo blanco

    # Dibujar los bloques del camino trazado por el jugador
    camino = estado_juego.get('caminoDibujado', [])
    for (cx, cy) in camino:
        pygame.draw.rect(pantalla, (80, 80, 80),
                        (cx*TAMANO_CELDA, cy*TAMANO_CELDA, TAMANO_CELDA, TAMANO_CELDA))

    # Dibujar la meta (casa) si existe
    meta = estado_juego.get('metaPos')
    if meta:
        rx, ry = meta
        coord_x = rx * TAMANO_CELDA
        coord_y = ry * TAMANO_CELDA

        if imagen_casa:
            # Si hay sprite, centrar la imagen en la celda
            rect_casa = imagen_casa.get_rect(center=(coord_x + TAMANO_CELDA//2, coord_y + TAMANO_CELDA//2))
            pantalla.blit(imagen_casa, rect_casa.topleft)
        else:
            # Dibujar un rectángulo verde como fallback
            pygame.draw.rect(pantalla, (0, 200, 0), (coord_x, coord_y, TAMANO_CELDA, TAMANO_CELDA))

    # Posición y orientación del coche desde el estado lógico
    coche_pos = estado_juego.get('cochePos', [0, 0])
    curr_x, curr_y = coche_pos
    angulo = estado_juego.get('angulo', 0.0)

    # Coordenadas de dibujo en píxeles; aplicar offset vertical si está cayendo
    visual_y = (curr_y * TAMANO_CELDA) + offset_caida_y
    visual_x = (curr_x * TAMANO_CELDA)

    if sprites_coche:
        # Usar sprites animados si están disponibles; rotar según ángulo lógico
        idx = int(frame_actual) % 4
        imagen_rotada = pygame.transform.rotate(sprites_coche[idx], -angulo)
        rect = imagen_rotada.get_rect(center=(visual_x + TAMANO_CELDA//2,
                                              visual_y + TAMANO_CELDA//2))
        pantalla.blit(imagen_rotada, rect.topleft)
    else:
        # Dibujo simple como fallback: círculo azul que representa el coche
        rect = pygame.Rect(visual_x, visual_y, TAMANO_CELDA, TAMANO_CELDA)
        pygame.draw.circle(pantalla, (0, 0, 255), rect.center, TAMANO_CELDA // 2)

    # Mostrar mensaje de estado (instrucciones o resultado)
    estado_logico = estado_juego.get('estado', '')
    font = pygame.font.SysFont("Arial", 22, bold=True)
    msg = ""
    col = (0,0,0)

    if estado_logico == 'Dibujando':
        msg = "Click en el coche y arrastra hacia la casa"
        col = (50, 50, 50)
    elif estado_logico == 'Ganado':
        msg = "¡LLEGASTE A CASA! [R] Nuevo Nivel"
        col = (0, 150, 0)
    elif estado_logico == 'Caido':
        # Mostrar mensaje distinto si la animación de caída ya se completó visualmente
        if juego_terminado_visualmente:
            msg = "¡Te caíste al vacío! [R] Reintentar"
            col = (200, 0, 0)
        else:
            msg = "¡Cayendooooo...!"
            col = (200, 100, 0)

    if msg:
        pantalla.blit(font.render(msg, True, col), (10, 10))

    pygame.display.flip()  # Actualizar pantalla completa


def main():
    # Ejecuta el proceso Haskell y gestiona la UI/Juego
    proc = None

    def iniciar_haskell():
        """
        Inicia el ejecutable Haskell como proceso hijo con pipes para stdin/stdout.
        Envía un primer TICK para solicitar el estado inicial y lee la respuesta JSON.
        Devuelve (process, estado_inicial_dict) o (None, None) en caso de error.
        """
        try:
            p = subprocess.Popen([HASKELL_EXECUTABLE], stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True, bufsize=0)
            # Pedir un estado inicial sincronizando con un comando TICK
            p.stdin.write("TICK\n")
            return p, json.loads(p.stdout.readline())
        except:
            return None, None

    proc, estado_actual = iniciar_haskell()
    if not proc:
        print("Error: Asegúrate de haber compilado Haskell.")
        return

    # Variables de control de la interfaz
    corriendo = True
    dibujando = False                # True mientras el usuario arrastra para dibujar el camino
    indice_animacion = 0             # Contador de frames para animación del coche
    acumulador_tiempo = 0

    animando_caida = False           # Cuando el coche está en animación de caída
    offset_caida_y = 0               # Desplazamiento vertical acumulado durante la caída
    juego_terminado_visualmente = False  # True cuando la animación final de caída terminó

    while corriendo:
        dt = reloj.tick(60)  # dt en ms desde el último frame (limita a ~60 FPS)

        # Manejo de eventos de Pygame (entrada del usuario)
        for evento in pygame.event.get():
            if evento.type == pygame.QUIT:
                corriendo = False

            elif evento.type == pygame.KEYDOWN and evento.key == pygame.K_r:
                # Reiniciar juego: terminar proceso viejo e iniciar uno nuevo
                try: proc.terminate()
                except: pass
                proc, estado_actual = iniciar_haskell()
                if not proc: corriendo = False
                dibujando = False
                indice_animacion = 0
                animando_caida = False
                offset_caida_y = 0
                juego_terminado_visualmente = False

            elif evento.type == pygame.MOUSEBUTTONDOWN and not animando_caida:
                # Empezar a dibujar solo si el estado lógico permite 'Dibujando'
                if estado_actual.get('estado') == 'Dibujando':
                    dibujando = True
                    try:
                        # Resetear el path en la lógica y enviar la primera coordenada
                        proc.stdin.write("RESET_PATH\n")
                        proc.stdout.readline()  # consumir respuesta de confirmación
                        mx, my = pygame.mouse.get_pos()
                        proc.stdin.write(f"DIBUJAR {mx//TAMANO_CELDA} {my//TAMANO_CELDA}\n")
                        estado_actual = json.loads(proc.stdout.readline())
                    except:
                        corriendo = False

            elif evento.type == pygame.MOUSEBUTTONUP and dibujando:
                # Cuando se suelta el botón, enviar INICIAR para que el coche comience
                dibujando = False
                try:
                    proc.stdin.write("INICIAR\n")
                    estado_actual = json.loads(proc.stdout.readline())
                except:
                    corriendo = False

        # Lógica principal: actualizar estado comunicándose con Haskell según el estado lógico
        try:
            estado_logico = estado_actual.get('estado')

            if dibujando and estado_logico == 'Dibujando':
                # Mientras arrastra, enviar coordenadas continuamente
                mx, my = pygame.mouse.get_pos()
                proc.stdin.write(f"DIBUJAR {mx//TAMANO_CELDA} {my//TAMANO_CELDA}\n")
                estado_actual = json.loads(proc.stdout.readline())

            elif estado_logico == 'EnCurso':
                # Durante el curso del movimiento, animar localmente y cada 4 frames pedir TICK
                acumulador_tiempo += dt
                if acumulador_tiempo >= DURACION_FRAME:
                    acumulador_tiempo = 0
                    indice_animacion += 1
                    # Cada 4 incrementos de índice animación (esto sincroniza lógica vs sprite)
                    if indice_animacion % 4 == 0:
                        proc.stdin.write("TICK\n")
                        estado_actual = json.loads(proc.stdout.readline())

            elif estado_logico == 'Caido' and not animando_caida and not juego_terminado_visualmente:
                # Cuando la lógica informa que cayó, empezar animación de caída visual
                animando_caida = True

            if animando_caida:
                # Animación de caída: acumular tiempo, aumentar offset y actualizar índice animación
                acumulador_tiempo += dt
                if acumulador_tiempo >= DURACION_FRAME:
                    acumulador_tiempo = 0
                    indice_animacion += 1

                offset_caida_y += VELOCIDAD_CAIDA

                # Cuando el coche sale de la pantalla por abajo, considerar la animación completa
                coche_y_pixeles = estado_actual.get('cochePos')[1] * TAMANO_CELDA
                if coche_y_pixeles + offset_caida_y > ALTO_VENTANA + 50:
                    animando_caida = False
                    juego_terminado_visualmente = True

        except Exception as e:
            # Problema en comunicación con el proceso Haskell: salir y avisar
            print(f"Error de comunicación: {e}")
            corriendo = False

        # Dibujar estado actual (si hay un estado)
        dibujar_juego(estado_actual, indice_animacion, offset_caida_y, juego_terminado_visualmente)

    # Al salir, intentar terminar el proceso hijo y cerrar Pygame
    try: proc.terminate()
    except: pass
    pygame.quit()


if __name__ == '__main__':
    main()