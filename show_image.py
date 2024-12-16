import curses
from PIL import Image

def display_image(stdscr, image_path):
    stdscr.clear()

    # Apri l'immagine
    img = Image.open(image_path)

    # Ottieni le dimensioni dell'immagine
    width, height = img.size

    # Calcola il numero di righe e colonne per la griglia
    rows, cols = stdscr.getmaxyx()

    # Limita le dimensioni della griglia se necessario
    grid_width = min(width, cols - 1)
    grid_height = min(height, rows - 1)

    # Scala l'immagine nella griglia
    scale_factor_x = grid_width / width
    scale_factor_y = grid_height / height
    scale_factor = min(scale_factor_x, scale_factor_y)

    scaled_width = int(width * scale_factor)
    scaled_height = int(height * scale_factor)

    # Crea una matrice di pixel
    pixels = []
    for y in range(scaled_height):
        row = []
        for x in range(scaled_width):
            color = img.getpixel((x, y))
            row.append(color)
        pixels.append(row)

    # Disegna i pixel nell'interfaccia ncurses
    curses.start_color()
    for y, row in enumerate(pixels):
        for x, color in enumerate(row):
            attr = curses.color_pair(1) | (curses.A_BOLD if (x + y) % 2 == 0 else 0)
            stdscr.addch(y, x, attr)

    stdscr.refresh()

# Inizializza ncurses
stdscr = curses.initscr()
curses.noecho()
curses.cbreak()
stdscr.keypad(True)

try:
    display_image(stdscr, "/home/simone/Codes/dots/themes/pngs/lain.png")
    import time
    time.sleep(3)
finally:
    curses.endwin()
