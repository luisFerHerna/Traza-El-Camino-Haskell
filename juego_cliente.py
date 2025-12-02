import subprocess
import pygame
import json
import sys
import os

HASKELL_EXECUTABLE = './juego_haskell'
ANCHO_VENTANA, ALTO_VENTANA = 800, 600
TAMANO_CELDA = 25
DURACION_FRAME = 80
VELOCIDAD_CAIDA = 9

COLOR_FONDO = (100, 215, 120)
COLOR_CAMINO = (80, 80, 90)
COLOR_UI_BG = (0, 0, 0, 150)
COLOR_HITBOX_OBS = (255, 50, 50, 60)
COLOR_HITBOX_META = (50, 50, 255, 60)

pygame.init()
pygame.mixer.init()

pantalla = pygame.display.set_mode((ANCHO_VENTANA, ALTO_VENTANA))
pygame.display.set_caption("LLEVA AL COCHE A CASA 🚗🏡")
reloj = pygame.time.Clock()

sprites_coche = []
imagen_casa = None
imagen_obstaculo = None
imagen_corazon = None
sonidos = {}

def cargar_recursos():
    global imagen_casa, imagen_obstaculo, imagen_corazon
    try:
        if os.path.exists('frames.png'):
            hoja = pygame.image.load('frames.png').convert_alpha()
            ancho_frame = hoja.get_width() // 4
            alto_hoja = hoja.get_height()
            for i in range(4):
                rect = pygame.Rect(i * ancho_frame, 0, ancho_frame, alto_hoja)
                frame = hoja.subsurface(rect)
                frame = pygame.transform.scale(frame, (int(TAMANO_CELDA * 2.5), int(TAMANO_CELDA * 2.5)))
                sprites_coche.append(frame)

        if os.path.exists('casa.png'):
            img = pygame.image.load('casa.png').convert_alpha()
            imagen_casa = pygame.transform.scale(img, (int(TAMANO_CELDA * 4.0), int(TAMANO_CELDA * 4.0)))

        if os.path.exists('obstaculo.png'):
            img = pygame.image.load('obstaculo.png').convert_alpha()
            imagen_obstaculo = pygame.transform.scale(img, (int(TAMANO_CELDA * 2.0), int(TAMANO_CELDA * 2.0)))

        if os.path.exists('corazon.png'):
            img = pygame.image.load('corazon.png').convert_alpha()
            imagen_corazon = pygame.transform.scale(img, (35, 35))

        lista_sonidos = {
            'ambientacion': 'ambientacion.mp3',
            'caida': 'caida.mp3',
            'victoria': 'victoria.mp3',
            'perder': 'perder.mp3',
            'choque': 'choque.mp3'
        }
        for nombre, archivo in lista_sonidos.items():
            if os.path.exists(archivo):
                sonidos[nombre] = pygame.mixer.Sound(archivo)
                if nombre == 'ambientacion':
                    sonidos[nombre].set_volume(0.4)
                if nombre == 'victoria':
                    sonidos[nombre].set_volume(0.6)
    except Exception as e:
        print(f"Nota: {e}")

cargar_recursos()

if 'ambientacion' in sonidos:
    sonidos['ambientacion'].play(loops=-1)

def dibujar_hitbox_obstaculos(pantalla, obstaculos, meta):
    hitbox_surf = pygame.Surface((ANCHO_VENTANA, ALTO_VENTANA), pygame.SRCALPHA)
    radio_hitbox = TAMANO_CELDA * 2 + (TAMANO_CELDA // 2)

    for (ox, oy) in obstaculos:
        center_x = ox * TAMANO_CELDA + TAMANO_CELDA // 2
        center_y = oy * TAMANO_CELDA + TAMANO_CELDA // 2
        pygame.draw.circle(hitbox_surf, COLOR_HITBOX_OBS, (center_x, center_y), radio_hitbox)

    if meta:
        center_x = meta[0] * TAMANO_CELDA + TAMANO_CELDA // 2
        center_y = meta[1] * TAMANO_CELDA + TAMANO_CELDA // 2
        pygame.draw.circle(hitbox_surf, COLOR_HITBOX_META, (center_x, center_y), radio_hitbox)

    pantalla.blit(hitbox_surf, (0, 0))

def dibujar_juego(estado_juego, frame_actual, offset_caida_y, juego_terminado_visualmente):
    pantalla.fill(COLOR_FONDO)

    camino = estado_juego.get('caminoDibujado', [])
    for (cx, cy) in camino:
        rect = pygame.Rect(cx*TAMANO_CELDA, cy*TAMANO_CELDA, TAMANO_CELDA, TAMANO_CELDA)
        pygame.draw.rect(pantalla, COLOR_CAMINO, rect, border_radius=5)
        pygame.draw.rect(pantalla, (100, 100, 110), rect, width=1, border_radius=5)

    obstaculos = estado_juego.get('obstaculos', [])
    meta = estado_juego.get('metaPos')
    dibujar_hitbox_obstaculos(pantalla, obstaculos, meta)

    for (ox, oy) in obstaculos:
        center_x = ox * TAMANO_CELDA + TAMANO_CELDA // 2
        center_y = oy * TAMANO_CELDA + TAMANO_CELDA // 2
        if imagen_obstaculo:
            rect_img = imagen_obstaculo.get_rect(center=(center_x, center_y))
            pantalla.blit(imagen_obstaculo, rect_img.topleft)
        else:
            pygame.draw.circle(pantalla, (200, 50, 50), (center_x, center_y), TAMANO_CELDA//2)
            pygame.draw.circle(pantalla, (100, 0, 0), (center_x, center_y), TAMANO_CELDA//2, width=2)

    if meta:
        rx, ry = meta
        center_x = rx * TAMANO_CELDA + TAMANO_CELDA // 2
        center_y = ry * TAMANO_CELDA + TAMANO_CELDA // 2
        if imagen_casa:
            rect_casa = imagen_casa.get_rect(center=(center_x, center_y - (imagen_casa.get_height()//3)))
            pantalla.blit(imagen_casa, rect_casa.topleft)
        else:
            pygame.draw.rect(pantalla, (0, 200, 0), (rx*TAMANO_CELDA, ry*TAMANO_CELDA, TAMANO_CELDA, TAMANO_CELDA))

    coche_pos = estado_juego.get('cochePos', [0, 0])
    curr_x, curr_y = coche_pos
    angulo = estado_juego.get('angulo', 0.0)

    visual_y = (curr_y * TAMANO_CELDA) + offset_caida_y
    visual_x = (curr_x * TAMANO_CELDA)
    center_v_x = visual_x + TAMANO_CELDA // 2
    center_v_y = visual_y + TAMANO_CELDA // 2

    if sprites_coche:
        idx = int(frame_actual) % 4
        imagen_rotada = pygame.transform.rotate(sprites_coche[idx], -angulo)
        rect = imagen_rotada.get_rect(center=(center_v_x, center_v_y))
        if offset_caida_y == 0:
            sombra = pygame.Surface((rect.width, rect.height), pygame.SRCALPHA)
            pygame.draw.ellipse(sombra, (0, 0, 0, 60), sombra.get_rect())
            pantalla.blit(sombra, (rect.x + 5, rect.y + 10))
        pantalla.blit(imagen_rotada, rect.topleft)
    else:
        pygame.draw.circle(pantalla, (0, 0, 255), (center_v_x, center_v_y), TAMANO_CELDA // 2)

    s = pygame.Surface((ANCHO_VENTANA, 50))
    s.set_alpha(180)
    s.fill((0,0,0))
    pantalla.blit(s, (0,0))

    vidas = estado_juego.get('vidas', 3)
    nivel = estado_juego.get('nivelActual', 1)
    font = pygame.font.SysFont("Arial", 24, bold=True)

    for i in range(vidas):
        if imagen_corazon:
            pantalla.blit(imagen_corazon, (20 + i * 40, 8))
        else:
            txt_vida = font.render("♥", True, (255, 50, 50))
            pantalla.blit(txt_vida, (20 + i * 30, 10))

    txt_nivel = font.render(f"NIVEL {nivel}", True, (255, 255, 255))
    rect_nivel = txt_nivel.get_rect(midright=(ANCHO_VENTANA - 20, 25))
    pantalla.blit(txt_nivel, rect_nivel)

    estado_logico = estado_juego.get('estado', '')
    msg = ""
    col = (255, 255, 255)
    bg_msg = None

    if estado_logico == 'Dibujando':
        msg = "DIBUJA EL CAMINO"
        col = (255, 255, 255)
    elif estado_logico == 'Ganado':
        if juego_terminado_visualmente:
            msg = "¡NIVEL COMPLETADO! [ESPACIO] Continuar"
            col = (100, 255, 100)
            bg_msg = (0, 0, 0, 200)
    elif estado_logico == 'Caido' or estado_logico == 'Chocado':
        if juego_terminado_visualmente:
            msg = "¡NO HAS PODIDO LLEGAR! [ESPACIO] Reintentar"
            col = (255, 100, 100)
            bg_msg = (0, 0, 0, 200)
    elif estado_logico == 'GameOver':
        overlay = pygame.Surface((ANCHO_VENTANA, ALTO_VENTANA), pygame.SRCALPHA)
        overlay.fill((50, 0, 0, 220))
        pantalla.blit(overlay, (0,0))
        font_go = pygame.font.SysFont("Arial", 60, bold=True)
        txt_go = font_go.render("GAME OVER", True, (255, 255, 255))
        pantalla.blit(txt_go, txt_go.get_rect(center=(ANCHO_VENTANA//2, ALTO_VENTANA//2 - 40)))
        font_sub = pygame.font.SysFont("Arial", 30)
        txt_sub = font_sub.render("Presiona [R] para reiniciar todo", True, (200, 200, 200))
        pantalla.blit(txt_sub, txt_sub.get_rect(center=(ANCHO_VENTANA//2, ALTO_VENTANA//2 + 40)))
        pygame.display.flip()
        return

    if msg:
        font_msg = pygame.font.SysFont("Arial", 28, bold=True)
        texto_render = font_msg.render(msg, True, col)
        if bg_msg:
            padding = 20
            bg_rect = texto_render.get_rect(center=(ANCHO_VENTANA//2, ALTO_VENTANA - 50))
            bg_rect.inflate_ip(padding*2, padding)
            shape_surf = pygame.Surface(bg_rect.size, pygame.SRCALPHA)
            shape_surf.fill(bg_msg)
            pantalla.blit(shape_surf, bg_rect.topleft)
        rect_msg = texto_render.get_rect(center=(ANCHO_VENTANA//2, ALTO_VENTANA - 50))
        pantalla.blit(texto_render, rect_msg)

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
        print("Error: Compila Haskell primero.")
        return

    corriendo = True
    dibujando = False
    indice_animacion = 0
    acumulador_tiempo = 0

    animando_caida = False
    offset_caida_y = 0
    juego_terminado_visualmente = False
    estado_anterior = ""

    while corriendo:
        dt = reloj.tick(60)
        estado_logico = estado_actual.get('estado')

        if estado_logico != estado_anterior:
            if estado_logico == 'Caido' and 'caida' in sonidos:
                sonidos['caida'].play()
            elif estado_logico == 'Chocado' and 'choque' in sonidos:
                sonidos['choque'].play()
            elif estado_logico == 'Ganado' and 'victoria' in sonidos:
                sonidos['victoria'].play()
            elif estado_logico == 'GameOver' and 'perder' in sonidos:
                sonidos['perder'].play()
            estado_anterior = estado_logico

        for evento in pygame.event.get():
            if evento.type == pygame.QUIT:
                corriendo = False
            elif evento.type == pygame.KEYDOWN:
                if evento.key == pygame.K_r:
                     try:
                        proc.stdin.write("REINICIAR_COMPLETO\n")
                        estado_actual = json.loads(proc.stdout.readline())
                        animando_caida = False
                        offset_caida_y = 0
                        juego_terminado_visualmente = False
                        estado_anterior = ""
                        estado_logico = estado_actual.get('estado') # ACTUALIZAR AQUI TAMBIEN
                     except: corriendo = False

                elif evento.key == pygame.K_SPACE:
                    if juego_terminado_visualmente:
                        try:
                            animando_caida = False
                            offset_caida_y = 0
                            juego_terminado_visualmente = False
                            dibujando = False
                            estado_anterior = ""

                            if estado_logico == 'Ganado':
                                proc.stdin.write("SIGUIENTE_NIVEL\n")
                            elif estado_logico in ['Caido', 'Chocado']:
                                proc.stdin.write("REINTENTAR_O_PERDER\n")

                            estado_actual = json.loads(proc.stdout.readline())

                            # --- CORRECCIÓN CRÍTICA ---
                            # Actualizamos 'estado_logico' inmediatamente con el nuevo estado ('Dibujando')
                            # para que la lógica de abajo NO piense que seguimos chocados/caídos.
                            estado_logico = estado_actual.get('estado')

                        except: corriendo = False

            elif evento.type == pygame.MOUSEBUTTONDOWN:
                if estado_logico == 'Dibujando' and not animando_caida:
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

            elif estado_logico in ['Caido', 'Chocado', 'Ganado']:
                if estado_logico == 'Ganado' and not juego_terminado_visualmente:
                    juego_terminado_visualmente = True

                elif estado_logico in ['Caido', 'Chocado'] and not animando_caida and not juego_terminado_visualmente:
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

        except Exception:
            corriendo = False

        dibujar_juego(estado_actual, indice_animacion, offset_caida_y, juego_terminado_visualmente)

    try: proc.terminate()
    except: pass
    pygame.quit()

if __name__ == '__main__':
    main()