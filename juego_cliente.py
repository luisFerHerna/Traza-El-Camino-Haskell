import subprocess
import pygame
import json
import sys
import os


HASKELL_EXECUTABLE = './juego_haskell'
ANCHO_VENTANA, ALTO_VENTANA = 700, 500
TAMANO_CELDA = 20
DURACION_FRAME = 80
VELOCIDAD_CAIDA = 8

pygame.init()
pantalla = pygame.display.set_mode((ANCHO_VENTANA, ALTO_VENTANA))
pygame.display.set_caption("Traza el camino - Versión Final")
reloj = pygame.time.Clock()

sprites_coche = []
imagen_casa = None

try:
    if os.path.exists('frames.png'):
        hoja = pygame.image.load('frames.png').convert_alpha()
        ancho_frame = hoja.get_width() // 4
        alto_hoja = hoja.get_height()
        for i in range(4):
            rect = pygame.Rect(i * ancho_frame, 0, ancho_frame, alto_hoja)
            frame = hoja.subsurface(rect)
            frame = pygame.transform.scale(frame, (TAMANO_CELDA + 12, TAMANO_CELDA + 12))
            sprites_coche.append(frame)

    if os.path.exists('casa.png'):
        img_casa = pygame.image.load('casa.png').convert_alpha()
        imagen_casa = pygame.transform.scale(img_casa, (TAMANO_CELDA + 15, TAMANO_CELDA + 15))

except Exception as e:
    print(f"Advertencia cargando imágenes: {e}")


def dibujar_juego(estado_juego, frame_actual, offset_caida_y, juego_terminado_visualmente):
    pantalla.fill((255, 255, 255))


    camino = estado_juego.get('caminoDibujado', [])
    for (cx, cy) in camino:
        pygame.draw.rect(pantalla, (80, 80, 80),
                        (cx*TAMANO_CELDA, cy*TAMANO_CELDA, TAMANO_CELDA, TAMANO_CELDA))

    meta = estado_juego.get('metaPos')
    if meta:
        rx, ry = meta
        coord_x = rx * TAMANO_CELDA
        coord_y = ry * TAMANO_CELDA

        if imagen_casa:
            rect_casa = imagen_casa.get_rect(center=(coord_x + TAMANO_CELDA//2, coord_y + TAMANO_CELDA//2))
            pantalla.blit(imagen_casa, rect_casa.topleft)
        else:
            pygame.draw.rect(pantalla, (0, 200, 0), (coord_x, coord_y, TAMANO_CELDA, TAMANO_CELDA))

    coche_pos = estado_juego.get('cochePos', [0, 0])
    curr_x, curr_y = coche_pos
    angulo = estado_juego.get('angulo', 0.0)

    visual_y = (curr_y * TAMANO_CELDA) + offset_caida_y
    visual_x = (curr_x * TAMANO_CELDA)

    if sprites_coche:
        idx = int(frame_actual) % 4
        imagen_rotada = pygame.transform.rotate(sprites_coche[idx], -angulo)
        rect = imagen_rotada.get_rect(center=(visual_x + TAMANO_CELDA//2,
                                              visual_y + TAMANO_CELDA//2))
        pantalla.blit(imagen_rotada, rect.topleft)
    else:
        rect = pygame.Rect(visual_x, visual_y, TAMANO_CELDA, TAMANO_CELDA)
        pygame.draw.circle(pantalla, (0, 0, 255), rect.center, TAMANO_CELDA // 2)

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
        if juego_terminado_visualmente:
            msg = "¡Te caíste al vacío! [R] Reintentar"
            col = (200, 0, 0)
        else:
            msg = "¡Cayendooooo...!"
            col = (200, 100, 0)

    if msg:
        pantalla.blit(font.render(msg, True, col), (10, 10))

    pygame.display.flip()

def main():
    proc = None
    def iniciar_haskell():
        try:
            p = subprocess.Popen([HASKELL_EXECUTABLE], stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True, bufsize=0)
            p.stdin.write("TICK\n")
            return p, json.loads(p.stdout.readline())
        except: return None, None

    proc, estado_actual = iniciar_haskell()
    if not proc:
        print("Error: Asegúrate de haber compilado Haskell.")
        return

    corriendo = True
    dibujando = False
    indice_animacion = 0
    acumulador_tiempo = 0

    animando_caida = False
    offset_caida_y = 0
    juego_terminado_visualmente = False

    while corriendo:
        dt = reloj.tick(60)

        for evento in pygame.event.get():
            if evento.type == pygame.QUIT:
                corriendo = False

            elif evento.type == pygame.KEYDOWN and evento.key == pygame.K_r:
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
                if estado_actual.get('estado') == 'Dibujando':
                    dibujando = True
                    try:
                        proc.stdin.write("RESET_PATH\n")
                        proc.stdout.readline()
                        mx, my = pygame.mouse.get_pos()
                        proc.stdin.write(f"DIBUJAR {mx//TAMANO_CELDA} {my//TAMANO_CELDA}\n")
                        estado_actual = json.loads(proc.stdout.readline())
                    except: corriendo = False

            elif evento.type == pygame.MOUSEBUTTONUP and dibujando:
                dibujando = False
                try:
                    proc.stdin.write("INICIAR\n")
                    estado_actual = json.loads(proc.stdout.readline())
                except: corriendo = False

        try:
            estado_logico = estado_actual.get('estado')

            if dibujando and estado_logico == 'Dibujando':
                mx, my = pygame.mouse.get_pos()
                proc.stdin.write(f"DIBUJAR {mx//TAMANO_CELDA} {my//TAMANO_CELDA}\n")
                estado_actual = json.loads(proc.stdout.readline())

            elif estado_logico == 'EnCurso':
                acumulador_tiempo += dt
                if acumulador_tiempo >= DURACION_FRAME:
                    acumulador_tiempo = 0
                    indice_animacion += 1
                    if indice_animacion % 4 == 0:
                        proc.stdin.write("TICK\n")
                        estado_actual = json.loads(proc.stdout.readline())

            elif estado_logico == 'Caido' and not animando_caida and not juego_terminado_visualmente:
                animando_caida = True

            if animando_caida:
                acumulador_tiempo += dt
                if acumulador_tiempo >= DURACION_FRAME:
                    acumulador_tiempo = 0
                    indice_animacion += 1

                offset_caida_y += VELOCIDAD_CAIDA

                coche_y_pixeles = estado_actual.get('cochePos')[1] * TAMANO_CELDA
                if coche_y_pixeles + offset_caida_y > ALTO_VENTANA + 50:
                    animando_caida = False
                    juego_terminado_visualmente = True

        except Exception as e:
            print(f"Error de comunicación: {e}")
            corriendo = False

        dibujar_juego(estado_actual, indice_animacion, offset_caida_y, juego_terminado_visualmente)

    try: proc.terminate()
    except: pass
    pygame.quit()

if __name__ == '__main__':
    main()