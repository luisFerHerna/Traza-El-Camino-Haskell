import subprocess  # Para ejecutar procesos externos
import pygame  # Biblioteca para crear videojuegos
import json  # Para manejar datos en formato JSON
import sys  # Para acceder a funciones y variables del intérprete
import os  # Para interactuar con el sistema operativo

# Definición de constantes
HASKELL_EXECUTABLE = './juego_haskell'  # Ruta al ejecutable de Haskell
ANCHO_VENTANA, ALTO_VENTANA = 800, 600  # Dimensiones de la ventana del juego
TAMANO_CELDA = 25  # Tamaño de cada celda en el juego
DURACION_FRAME = 80  # Duración de cada frame en milisegundos
VELOCIDAD_CAIDA = 9  # Velocidad de caída del coche

# Definición de colores
COLOR_FONDO = (100, 215, 120)  # Color de fondo
COLOR_CAMINO = (80, 80, 90)  # Color del camino
COLOR_UI_BG = (0, 0, 0, 150)  # Color de fondo de la interfaz
COLOR_HITBOX_OBS = (255, 50, 50, 60)  # Color de la hitbox de obstáculos
COLOR_HITBOX_META = (50, 50, 255, 60)  # Color de la hitbox de la meta

# Inicialización de Pygame
pygame.init()
pygame.mixer.init()  # Inicialización del mezclador de sonido

# Configuración de la pantalla
pantalla = pygame.display.set_mode((ANCHO_VENTANA, ALTO_VENTANA))
pygame.display.set_caption("LLEVA AL COCHE A CASA 🚗🏡")  # Título de la ventana
reloj = pygame.time.Clock()  # Reloj para controlar la velocidad del juego

# Listas para almacenar imágenes y sonidos
sprites_coche = []  # Lista de sprites del coche
imagen_casa = None  # Imagen de la casa
imagen_obstaculo = None  # Imagen del obstáculo
imagen_corazon = None  # Imagen del corazón
sonidos = {}  # Diccionario para almacenar sonidos

def cargar_recursos():
    global imagen_casa, imagen_obstaculo, imagen_corazon  # Variables globales
    try:
        # Carga de sprites del coche
        if os.path.exists('frames.png'):
            hoja = pygame.image.load('frames.png').convert_alpha()  # Carga la hoja de sprites
            ancho_frame = hoja.get_width() // 4  # Ancho de cada frame
            alto_hoja = hoja.get_height()  # Alto de la hoja
            for i in range(4):
                rect = pygame.Rect(i * ancho_frame, 0, ancho_frame, alto_hoja)  # Rectángulo para cada frame
                frame = hoja.subsurface(rect)  # Extrae el frame
                frame = pygame.transform.scale(frame, (int(TAMANO_CELDA * 2.5), int(TAMANO_CELDA * 2.5)))  # Escala el frame
                sprites_coche.append(frame)  # Añade el frame a la lista

        # Carga de la imagen de la casa
        if os.path.exists('casa.png'):
            img = pygame.image.load('casa.png').convert_alpha()  # Carga la imagen de la casa
            imagen_casa = pygame.transform.scale(img, (int(TAMANO_CELDA * 4.0), int(TAMANO_CELDA * 4.0)))  # Escala la imagen

        # Carga de la imagen del obstáculo
        if os.path.exists('obstaculo.png'):
            img = pygame.image.load('obstaculo.png').convert_alpha()  # Carga la imagen del obstáculo
            imagen_obstaculo = pygame.transform.scale(img, (int(TAMANO_CELDA * 2.0), int(TAMANO_CELDA * 2.0)))  # Escala la imagen

        # Carga de la imagen del corazón
        if os.path.exists('corazon.png'):
            img = pygame.image.load('corazon.png').convert_alpha()  # Carga la imagen del corazón
            imagen_corazon = pygame.transform.scale(img, (35, 35))  # Escala la imagen

        # Carga de sonidos
        lista_sonidos = {
            'ambientacion': 'ambientacion.mp3',
            'caida': 'caida.mp3',
            'victoria': 'victoria.mp3',
            'perder': 'perder.mp3',
            'choque': 'choque.mp3'
        }
        for nombre, archivo in lista_sonidos.items():
            if os.path.exists(archivo):
                sonidos[nombre] = pygame.mixer.Sound(archivo)  # Carga el sonido
                if nombre == 'ambientacion':
                    sonidos[nombre].set_volume(0.4)  # Ajusta el volumen de la ambientación
                if nombre == 'victoria':
                    sonidos[nombre].set_volume(0.6)  # Ajusta el volumen de la victoria
    except Exception as e:
        print(f"Nota: {e}")  # Manejo de excepciones

cargar_recursos()  # Llama a la función para cargar recursos

# Reproduce el sonido de ambientación en bucle
if 'ambientacion' in sonidos:
    sonidos['ambientacion'].play(loops=-1)

def dibujar_hitbox_obstaculos(pantalla, obstaculos, meta):
    # Dibuja las hitboxes de los obstáculos y la meta
    hitbox_surf = pygame.Surface((ANCHO_VENTANA, ALTO_VENTANA), pygame.SRCALPHA)  # Superficie para las hitboxes
    radio_hitbox = TAMANO_CELDA * 2 + (TAMANO_CELDA // 2)  # Radio de las hitboxes

    # Dibuja las hitboxes de los obstáculos
    for (ox, oy) in obstaculos:
        center_x = ox * TAMANO_CELDA + TAMANO_CELDA // 2  # Coordenada X del centro
        center_y = oy * TAMANO_CELDA + TAMANO_CELDA // 2  # Coordenada Y del centro
        pygame.draw.circle(hitbox_surf, COLOR_HITBOX_OBS, (center_x, center_y), radio_hitbox)  # Dibuja el círculo

    # Dibuja la hitbox de la meta
    if meta:
        center_x = meta[0] * TAMANO_CELDA + TAMANO_CELDA // 2  # Coordenada X del centro
        center_y = meta[1] * TAMANO_CELDA + TAMANO_CELDA // 2  # Coordenada Y del centro
        pygame.draw.circle(hitbox_surf, COLOR_HITBOX_META, (center_x, center_y), radio_hitbox)  # Dibuja el círculo

    pantalla.blit(hitbox_surf, (0, 0))  # Dibuja la superficie de hitboxes en la pantalla

def dibujar_juego(estado_juego, frame_actual, offset_caida_y, juego_terminado_visualmente):
    # Dibuja el estado actual del juego en la pantalla
    pantalla.fill(COLOR_FONDO)  # Rellena la pantalla con el color de fondo

    camino = estado_juego.get('caminoDibujado', [])  # Obtiene el camino dibujado
    for (cx, cy) in camino:
        rect = pygame.Rect(cx*TAMANO_CELDA, cy*TAMANO_CELDA, TAMANO_CELDA, TAMANO_CELDA)  # Crea un rectángulo para el camino
        pygame.draw.rect(pantalla, COLOR_CAMINO, rect, border_radius=5)  # Dibuja el camino
        pygame.draw.rect(pantalla, (100, 100, 110), rect, width=1, border_radius=5)  # Dibuja el borde del camino

    obstaculos = estado_juego.get('obstaculos', [])  # Obtiene la lista de obstáculos
    meta = estado_juego.get('metaPos')  # Obtiene la posición de la meta
    dibujar_hitbox_obstaculos(pantalla, obstaculos, meta)  # Dibuja las hitboxes de obstáculos y meta

    # Dibuja los obstáculos en la pantalla
    for (ox, oy) in obstaculos:
        center_x = ox * TAMANO_CELDA + TAMANO_CELDA // 2  # Coordenada X del centro
        center_y = oy * TAMANO_CELDA + TAMANO_CELDA // 2  # Coordenada Y del centro
        if imagen_obstaculo:
            rect_img = imagen_obstaculo.get_rect(center=(center_x, center_y))  # Obtiene el rectángulo de la imagen del obstáculo
            pantalla.blit(imagen_obstaculo, rect_img.topleft)  # Dibuja la imagen del obstáculo
        else:
            pygame.draw.circle(pantalla, (200, 50, 50), (center_x, center_y), TAMANO_CELDA//2)  # Dibuja un círculo si no hay imagen
            pygame.draw.circle(pantalla, (100, 0, 0), (center_x, center_y), TAMANO_CELDA//2, width=2)  # Dibuja el borde del círculo

    # Dibuja la meta en la pantalla
    if meta:
        rx, ry = meta
        center_x = rx * TAMANO_CELDA + TAMANO_CELDA // 2  # Coordenada X del centro
        center_y = ry * TAMANO_CELDA + TAMANO_CELDA // 2  # Coordenada Y del centro
        if imagen_casa:
            rect_casa = imagen_casa.get_rect(center=(center_x, center_y - (imagen_casa.get_height()//3)))  # Obtiene el rectángulo de la imagen de la casa
            pantalla.blit(imagen_casa, rect_casa.topleft)  # Dibuja la imagen de la casa
        else:
            pygame.draw.rect(pantalla, (0, 200, 0), (rx*TAMANO_CELDA, ry*TAMANO_CELDA, TAMANO_CELDA, TAMANO_CELDA))  # Dibuja un rectángulo si no hay imagen

    coche_pos = estado_juego.get('cochePos', [0, 0])  # Obtiene la posición del coche
    curr_x, curr_y = coche_pos  # Coordenadas actuales del coche
    angulo = estado_juego.get('angulo', 0.0)  # Obtiene el ángulo del coche

    visual_y = (curr_y * TAMANO_CELDA) + offset_caida_y  # Calcula la posición Y visual del coche
    visual_x = (curr_x * TAMANO_CELDA)  # Calcula la posición X visual del coche
    center_v_x = visual_x + TAMANO_CELDA // 2  # Centro X visual
    center_v_y = visual_y + TAMANO_CELDA // 2  # Centro Y visual

    # Dibuja el coche en la pantalla
    if sprites_coche:
        idx = int(frame_actual) % 4  # Índice del sprite actual
        imagen_rotada = pygame.transform.rotate(sprites_coche[idx], -angulo)  # Rota la imagen del coche
        rect = imagen_rotada.get_rect(center=(center_v_x, center_v_y))  # Obtiene el rectángulo de la imagen rotada
        if offset_caida_y == 0:
            sombra = pygame.Surface((rect.width, rect.height), pygame.SRCALPHA)  # Superficie para la sombra
            pygame.draw.ellipse(sombra, (0, 0, 0, 60), sombra.get_rect())  # Dibuja la sombra
            pantalla.blit(sombra, (rect.x + 5, rect.y + 10))  # Dibuja la sombra en la pantalla
        pantalla.blit(imagen_rotada, rect.topleft)  # Dibuja la imagen rotada del coche
    else:
        pygame.draw.circle(pantalla, (0, 0, 255), (center_v_x, center_v_y), TAMANO_CELDA // 2)  # Dibuja un círculo si no hay sprites

    # Dibuja la interfaz de usuario
    s = pygame.Surface((ANCHO_VENTANA, 50))  # Superficie para la barra de UI
    s.set_alpha(180)  # Ajusta la transparencia
    s.fill((0,0,0))  # Rellena con color negro
    pantalla.blit(s, (0,0))  # Dibuja la superficie en la pantalla

    vidas = estado_juego.get('vidas', 3)  # Obtiene el número de vidas
    nivel = estado_juego.get('nivelActual', 1)  # Obtiene el nivel actual
    font = pygame.font.SysFont("Arial", 24, bold=True)  # Fuente para el texto

    # Dibuja las vidas en la pantalla
    for i in range(vidas):
        if imagen_corazon:
            pantalla.blit(imagen_corazon, (20 + i * 40, 8))  # Dibuja la imagen del corazón
        else:
            txt_vida = font.render("♥", True, (255, 50, 50))  # Dibuja un corazón como texto
            pantalla.blit(txt_vida, (20 + i * 30, 10))  # Dibuja el texto en la pantalla

    txt_nivel = font.render(f"NIVEL {nivel}", True, (255, 255, 255))  # Dibuja el texto del nivel
    rect_nivel = txt_nivel.get_rect(midright=(ANCHO_VENTANA - 20, 25))  # Obtiene el rectángulo del texto del nivel
    pantalla.blit(txt_nivel, rect_nivel)  # Dibuja el texto del nivel en la pantalla

    estado_logico = estado_juego.get('estado', '')  # Obtiene el estado lógico del juego
    msg = ""  # Mensaje a mostrar
    col = (255, 255, 255)  # Color del mensaje
    bg_msg = None  # Fondo del mensaje

    # Determina el mensaje a mostrar según el estado del juego
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
        overlay = pygame.Surface((ANCHO_VENTANA, ALTO_VENTANA), pygame.SRCALPHA)  # Superficie para el overlay
        overlay.fill((50, 0, 0, 220))  # Rellena con color rojo oscuro
        pantalla.blit(overlay, (0,0))  # Dibuja el overlay en la pantalla
        font_go = pygame.font.SysFont("Arial", 60, bold=True)  # Fuente para el texto de Game Over
        txt_go = font_go.render("GAME OVER", True, (255, 255, 255))  # Dibuja el texto de Game Over
        pantalla.blit(txt_go, txt_go.get_rect(center=(ANCHO_VENTANA//2, ALTO_VENTANA//2 - 40)))  # Dibuja el texto en el centro
        font_sub = pygame.font.SysFont("Arial", 30)  # Fuente para el texto secundario
        txt_sub = font_sub.render("Presiona [R] para reiniciar todo", True, (200, 200, 200))  # Dibuja el texto secundario
        pantalla.blit(txt_sub, txt_sub.get_rect(center=(ANCHO_VENTANA//2, ALTO_VENTANA//2 + 40)))  # Dibuja el texto en el centro
        pygame.display.flip()  # Actualiza la pantalla
        return  # Sale de la función

    # Dibuja el mensaje en la pantalla
    if msg:
        font_msg = pygame.font.SysFont("Arial", 28, bold=True)  # Fuente para el mensaje
        texto_render = font_msg.render(msg, True, col)  # Renderiza el mensaje
        if bg_msg:
            padding = 20  # Padding para el fondo del mensaje
            bg_rect = texto_render.get_rect(center=(ANCHO_VENTANA//2, ALTO_VENTANA - 50))  # Rectángulo del mensaje
            bg_rect.inflate_ip(padding*2, padding)  # Aumenta el tamaño del rectángulo
            shape_surf = pygame.Surface(bg_rect.size, pygame.SRCALPHA)  # Superficie para el fondo
            shape_surf.fill(bg_msg)  # Rellena con el color de fondo
            pantalla.blit(shape_surf, bg_rect.topleft)  # Dibuja el fondo en la pantalla
        rect_msg = texto_render.get_rect(center=(ANCHO_VENTANA//2, ALTO_VENTANA - 50))  # Rectángulo del mensaje
        pantalla.blit(texto_render, rect_msg)  # Dibuja el mensaje en la pantalla

    pygame.display.flip()  # Actualiza la pantalla

def main():
    proc = None  # Proceso de Haskell

    def iniciar_haskell():
        # Inicia el proceso de Haskell y devuelve el proceso y el estado inicial
        try:
            p = subprocess.Popen([HASKELL_EXECUTABLE], stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True, bufsize=0)  # Inicia el proceso
            p.stdin.write("TICK\n")  # Envía un tick inicial
            return p, json.loads(p.stdout.readline())  # Devuelve el proceso y el estado inicial
        except: return None, None  # Manejo de excepciones