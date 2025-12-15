import subprocess
import pygame
import json
import sys
import os
import math

# ============================================================================
# CONSTANTES
# ============================================================================
HASKELL_EXECUTABLE = './juego_haskell'
ARCHIVO_PUNTAJES = 'puntajes.json'
ANCHO_VENTANA, ALTO_VENTANA = 800, 600
TAMANO_CELDA = 25
DURACION_FRAME = 80
VELOCIDAD_CAIDA = 9
ALTO_INTERFAZ = 110 # Altura de la zona prohibida para dibujar

# ============================================================================
# COLORES
# ============================================================================
COLOR_FONDO = (100, 215, 120)
COLOR_GRILLA = (80, 180, 100)
COLOR_CAMINO = (80, 80, 90)
COLOR_HITBOX_OBS = (255, 50, 50, 60)
COLOR_HITBOX_META = (50, 50, 255, 60)

# Colores UI
COLOR_BARRA_SUPERIOR = (30, 30, 35) 
COLOR_BARRA_FONDO = (60, 60, 60)
COLOR_BARRA_RELLENO = (255, 140, 0)
COLOR_ESTRELLA_ON = (255, 215, 0)
COLOR_ESTRELLA_OFF = (80, 80, 80)
COLOR_TABLA_BG = (30, 30, 40, 240)

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

# ============================================================================
# GESTIÓN DE PUNTAJES (JSON)
# ============================================================================
def guardar_puntaje(nombre, nivel, estrellas):
    datos = []
    if os.path.exists(ARCHIVO_PUNTAJES):
        try:
            with open(ARCHIVO_PUNTAJES, 'r') as f:
                datos = json.load(f)
        except: pass
    
    datos.append({"nombre": nombre, "nivel": nivel, "estrellas": estrellas})
    datos.sort(key=lambda x: x['estrellas'], reverse=True)
    datos = datos[:5]
    
    with open(ARCHIVO_PUNTAJES, 'w') as f:
        json.dump(datos, f)

def obtener_mejores_puntajes():
    if os.path.exists(ARCHIVO_PUNTAJES):
        try:
            with open(ARCHIVO_PUNTAJES, 'r') as f:
                return json.load(f)
        except: return []
    return []

# ============================================================================
# LÓGICA DE ESTRELLAS
# ============================================================================
def calcular_estrellas_ganadas(longitud_usada, longitud_optima):
    if longitud_optima == 0: return 0
    limite_3 = longitud_optima * 1.2
    limite_2 = longitud_optima * 1.6
    limite_1 = longitud_optima * 2.2
    
    if longitud_usada <= limite_3: return 3
    elif longitud_usada <= limite_2: return 2
    elif longitud_usada <= limite_1: return 1
    else: return 0

def dibujar_estrella_shape(surface, color, center_x, center_y, radio, borde=False):
    points = []
    for i in range(10):
        angle = math.radians(i * 36 - 90)
        r = radio if i % 2 == 0 else radio * 0.45
        x = center_x + r * math.cos(angle)
        y = center_y + r * math.sin(angle)
        points.append((x, y))
    pygame.draw.polygon(surface, color, points)
    if borde:
        pygame.draw.polygon(surface, (0,0,0), points, 1)

def dibujar_barra_estrellas(pantalla, longitud_usada, longitud_optima):
    if longitud_optima == 0: return 

    limite_3 = longitud_optima * 1.2
    limite_2 = longitud_optima * 1.6
    max_presupuesto = longitud_optima * 2.2
    
    ancho_total_barra = 400
    alto_barra = 15
    x_barra = (ANCHO_VENTANA // 2) - (ancho_total_barra // 2)
    y_barra = 60 
    
    rect_fondo = pygame.Rect(x_barra, y_barra, ancho_total_barra, alto_barra)
    pygame.draw.rect(pantalla, COLOR_BARRA_FONDO, rect_fondo, border_radius=10)
    pygame.draw.rect(pantalla, (200, 200, 200), rect_fondo, 2, border_radius=10)

    porcentaje_llenado = min(1.0, longitud_usada / max_presupuesto)
    ancho_llenado = int(ancho_total_barra * porcentaje_llenado)
    rect_relleno = pygame.Rect(x_barra, y_barra, ancho_llenado, alto_barra)
    pygame.draw.rect(pantalla, COLOR_BARRA_RELLENO, rect_relleno, border_radius=10)

    pos_star_3 = (limite_3 / max_presupuesto) * ancho_total_barra
    x_s3 = x_barra + pos_star_3
    
    pos_star_2 = (limite_2 / max_presupuesto) * ancho_total_barra
    x_s2 = x_barra + pos_star_2
    
    pos_star_1 = 0.95 * ancho_total_barra
    x_s1 = x_barra + pos_star_1

    y_stars = y_barra + 25
    radio_estrella = 12

    col_s3 = COLOR_ESTRELLA_ON if ancho_llenado < pos_star_3 else COLOR_ESTRELLA_OFF
    dibujar_estrella_shape(pantalla, col_s3, x_s3, y_stars, radio_estrella, borde=True)

    col_s2 = COLOR_ESTRELLA_ON if ancho_llenado < pos_star_2 else COLOR_ESTRELLA_OFF
    dibujar_estrella_shape(pantalla, col_s2, x_s2, y_stars, radio_estrella, borde=True)

    col_s1 = COLOR_ESTRELLA_ON if ancho_llenado < pos_star_1 else COLOR_ESTRELLA_OFF
    dibujar_estrella_shape(pantalla, col_s1, x_s1, y_stars, radio_estrella, borde=True)
    
    pygame.draw.line(pantalla, (255, 255, 255), (x_s3, y_barra), (x_s3, y_barra + alto_barra), 2)
    pygame.draw.line(pantalla, (255, 255, 255), (x_s2, y_barra), (x_s2, y_barra + alto_barra), 2)

    font_lbl = pygame.font.SysFont("Arial", 10, bold=True)
    render_lbl = font_lbl.render("COSTO RUTA", True, (200, 200, 200))
    pantalla.blit(render_lbl, (x_barra, y_barra - 15))

# ============================================================================
# DIBUJAR JUEGO
# ============================================================================
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

def dibujar_juego(estado_juego, frame_actual, offset_caida_y, juego_terminado_visualmente, longitud_fija_visual, estrellas_acumuladas_sesion):
    pantalla.fill(COLOR_FONDO)

    # Grilla
    for x in range(0, ANCHO_VENTANA, TAMANO_CELDA):
        pygame.draw.line(pantalla, COLOR_GRILLA, (x, 0), (x, ALTO_VENTANA), 1)
    for y in range(0, ALTO_VENTANA, TAMANO_CELDA):
        pygame.draw.line(pantalla, COLOR_GRILLA, (0, y), (ANCHO_VENTANA, y), 1)

    # Camino
    camino = estado_juego.get('caminoDibujado', [])
    for (cx, cy) in camino:
        rect = pygame.Rect(cx*TAMANO_CELDA, cy*TAMANO_CELDA, TAMANO_CELDA, TAMANO_CELDA)
        pygame.draw.rect(pantalla, COLOR_CAMINO, rect, border_radius=5)
        pygame.draw.rect(pantalla, (100, 100, 110), rect, width=1, border_radius=5)

    # Elementos
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

    if meta:
        rx, ry = meta
        center_x = rx * TAMANO_CELDA + TAMANO_CELDA // 2
        center_y = ry * TAMANO_CELDA + TAMANO_CELDA // 2
        if imagen_casa:
            rect_casa = imagen_casa.get_rect(center=(center_x, center_y - (imagen_casa.get_height()//3)))
            pantalla.blit(imagen_casa, rect_casa.topleft)
        else:
            pygame.draw.rect(pantalla, (0, 200, 0), (rx*TAMANO_CELDA, ry*TAMANO_CELDA, TAMANO_CELDA, TAMANO_CELDA))

    # Coche
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

    # UI SUPERIOR SÓLIDA
    pygame.draw.rect(pantalla, COLOR_BARRA_SUPERIOR, (0, 0, ANCHO_VENTANA, ALTO_INTERFAZ))
    pygame.draw.line(pantalla, (150, 150, 150), (0, ALTO_INTERFAZ), (ANCHO_VENTANA, ALTO_INTERFAZ), 3)

    vidas = estado_juego.get('vidas', 3)
    nivel = estado_juego.get('nivelActual', 1)
    font = pygame.font.SysFont("Arial", 24, bold=True)

    for i in range(vidas):
        if imagen_corazon:
            pantalla.blit(imagen_corazon, (20 + i * 40, 20))
        else:
            txt_vida = font.render("♥", True, (255, 50, 50))
            pantalla.blit(txt_vida, (20 + i * 30, 20))

    txt_nivel = font.render(f"NIVEL {nivel}", True, (255, 255, 255))
    rect_nivel = txt_nivel.get_rect(midright=(ANCHO_VENTANA - 20, 35))
    pantalla.blit(txt_nivel, rect_nivel)
    
    txt_score = font.render(f"★ Total: {estrellas_acumuladas_sesion}", True, (255, 215, 0))
    pantalla.blit(txt_score, (ANCHO_VENTANA // 2 - txt_score.get_width()//2, 15))

    distancia_optima = estado_juego.get('distanciaMinima', 1)
    estado_logico = estado_juego.get('estado', '')
    
    longitud_a_mostrar = 0
    if estado_logico == 'Dibujando':
        longitud_a_mostrar = len(estado_juego.get('caminoDibujado', []))
    else:
        longitud_a_mostrar = longitud_fija_visual

    if estado_logico in ['Dibujando', 'EnCurso', 'Ganado', 'Chocado', 'Caido']:
         dibujar_barra_estrellas(pantalla, longitud_a_mostrar, distancia_optima)

    msg = ""
    col = (255, 255, 255)
    bg_msg = None

    if estado_logico == 'Dibujando':
        msg = "DIBUJA EL CAMINO"
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

# ============================================================================
# PANTALLA GAME OVER E INPUT
# ============================================================================
def pantalla_game_over_input(nivel_alcanzado, estrellas_totales):
    nombre_ingresado = ""
    guardado = False
    
    fuente_titulo = pygame.font.SysFont("Arial", 60, bold=True)
    fuente_texto = pygame.font.SysFont("Arial", 30)
    fuente_tabla = pygame.font.SysFont("Arial", 20)

    while True:
        reloj.tick(30)
        
        for evento in pygame.event.get():
            if evento.type == pygame.QUIT:
                return False
            
            if not guardado and evento.type == pygame.KEYDOWN:
                if evento.key == pygame.K_RETURN:
                    if len(nombre_ingresado) > 0:
                        guardar_puntaje(nombre_ingresado, nivel_alcanzado, estrellas_totales)
                        guardado = True
                elif evento.key == pygame.K_BACKSPACE:
                    nombre_ingresado = nombre_ingresado[:-1]
                else:
                    if len(nombre_ingresado) < 10 and evento.unicode.isprintable():
                        nombre_ingresado += evento.unicode
            
            if guardado and evento.type == pygame.KEYDOWN:
                if evento.key == pygame.K_r:
                    return True

        overlay = pygame.Surface((ANCHO_VENTANA, ALTO_VENTANA), pygame.SRCALPHA)
        overlay.fill((20, 0, 0, 10))
        pantalla.blit(overlay, (0,0))
        
        panel_rect = pygame.Rect(100, 50, 600, 500)
        shape_surf = pygame.Surface(panel_rect.size, pygame.SRCALPHA)
        shape_surf.fill(COLOR_TABLA_BG)
        pantalla.blit(shape_surf, panel_rect.topleft)
        pygame.draw.rect(pantalla, (255, 50, 50), panel_rect, 2)

        txt_go = fuente_titulo.render("GAME OVER", True, (255, 50, 50))
        pantalla.blit(txt_go, txt_go.get_rect(center=(ANCHO_VENTANA//2, 100)))

        info = f"Nivel: {nivel_alcanzado}  |  Estrellas Totales: {estrellas_totales} ★"
        txt_info = fuente_texto.render(info, True, (200, 200, 200))
        pantalla.blit(txt_info, txt_info.get_rect(center=(ANCHO_VENTANA//2, 160)))

        if not guardado:
            txt_inst = fuente_texto.render("Escribe tu nombre y presiona ENTER:", True, (255, 255, 255))
            pantalla.blit(txt_inst, txt_inst.get_rect(center=(ANCHO_VENTANA//2, 220)))
            
            box_rect = pygame.Rect(ANCHO_VENTANA//2 - 100, 250, 200, 40)
            pygame.draw.rect(pantalla, (255, 255, 255), box_rect)
            pygame.draw.rect(pantalla, (0, 0, 0), box_rect, 2)
            
            txt_nombre = fuente_texto.render(nombre_ingresado, True, (0, 0, 0))
            pantalla.blit(txt_nombre, (box_rect.x + 10, box_rect.y + 5))
            
        else:
            txt_top = fuente_texto.render("🏆 MEJORES JUGADORES 🏆", True, (255, 215, 0))
            pantalla.blit(txt_top, txt_top.get_rect(center=(ANCHO_VENTANA//2, 210)))
            
            mejores = obtener_mejores_puntajes()
            start_y = 260
            pygame.draw.line(pantalla, (100,100,100), (150, start_y-10), (650, start_y-10))
            encabezado = fuente_tabla.render(f"{'NOMBRE':<15} {'NIVEL':<10} {'ESTRELLAS'}", True, (150, 150, 150))
            pantalla.blit(encabezado, (200, 240))

            for idx, p in enumerate(mejores):
                col = (255, 255, 255)
                if p['nombre'] == nombre_ingresado and p['estrellas'] == estrellas_totales:
                    col = (100, 255, 100) 

                fila_txt = f"{idx+1}. {p['nombre']:<15} {p['nivel']:<10} {p['estrellas']} ★"
                render_fila = fuente_tabla.render(fila_txt, True, col)
                pantalla.blit(render_fila, (200, start_y + idx * 30))

            txt_reiniciar = fuente_texto.render("Presiona [R] para volver a jugar", True, (255, 255, 255))
            if (pygame.time.get_ticks() // 500) % 2 == 0:
                pantalla.blit(txt_reiniciar, txt_reiniciar.get_rect(center=(ANCHO_VENTANA//2, 500)))

        pygame.display.flip()

# ============================================================================
# MAIN
# ============================================================================
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
    longitud_fija_visual = 0
    
    estrellas_acumuladas_sesion = 0
    estrellas_nivel_actual_calculadas = False 

    while corriendo:
        dt = reloj.tick(60)
        estado_logico = estado_actual.get('estado')

        if estado_logico == 'Dibujando':
            camino = estado_actual.get('caminoDibujado', [])
            longitud_fija_visual = len(camino)
            estrellas_nivel_actual_calculadas = False
        
        if estado_logico == 'Ganado' and not estrellas_nivel_actual_calculadas:
            dist = estado_actual.get('distanciaMinima', 1)
            ganadas = calcular_estrellas_ganadas(longitud_fija_visual, dist)
            estrellas_acumuladas_sesion += ganadas
            estrellas_nivel_actual_calculadas = True

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

        if estado_logico == 'GameOver':
            dibujar_juego(estado_actual, indice_animacion, offset_caida_y, juego_terminado_visualmente, longitud_fija_visual, estrellas_acumuladas_sesion)
            nivel_final = estado_actual.get('nivelActual', 1)
            
            quiere_reiniciar = pantalla_game_over_input(nivel_final, estrellas_acumuladas_sesion)
            
            if quiere_reiniciar:
                try:
                    proc.stdin.write("REINICIAR_COMPLETO\n")
                    estado_actual = json.loads(proc.stdout.readline())
                    animando_caida = False
                    offset_caida_y = 0
                    juego_terminado_visualmente = False
                    estado_anterior = ""
                    longitud_fija_visual = 0
                    estrellas_acumuladas_sesion = 0
                    estrellas_nivel_actual_calculadas = False
                    estado_logico = estado_actual.get('estado')
                except: corriendo = False
            else:
                corriendo = False
            continue

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
                        longitud_fija_visual = 0
                        estrellas_acumuladas_sesion = 0
                        estrellas_nivel_actual_calculadas = False
                        estado_logico = estado_actual.get('estado')
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
                            estado_logico = estado_actual.get('estado')
                            longitud_fija_visual = 0
                            
                        except: corriendo = False

            elif evento.type == pygame.MOUSEBUTTONDOWN:
                if estado_logico == 'Dibujando' and not animando_caida:
                    mx, my = pygame.mouse.get_pos()
                    # ZONA PROHIBIDA: Si el click es muy arriba (en la barra), no dibujar
                    if my > ALTO_INTERFAZ:
                        dibujando = True
                        try:
                            proc.stdin.write("RESET_PATH\n")
                            proc.stdout.readline()
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
                # ZONA PROHIBIDA: Si arrastras muy arriba, ignorar
                if my > ALTO_INTERFAZ:
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

        dibujar_juego(estado_actual, indice_animacion, offset_caida_y, juego_terminado_visualmente, longitud_fija_visual, estrellas_acumuladas_sesion)

    try: proc.terminate()
    except: pass
    pygame.quit()

if __name__ == '__main__':
    main()