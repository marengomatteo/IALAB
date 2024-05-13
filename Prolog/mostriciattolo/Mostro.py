import pygame
import random
import sys

# Definizione dei colori
NERO = (0, 0, 0)
BIANCO = (255, 255, 255)
VERDE = (0, 255, 0)
ROSSO = (255, 0, 0)
GIALLO = (255, 255, 102)
# Definizione delle costanti per le dimensioni della finestra e del labirinto
LARGHEZZA_FINESTRA = 800
ALTEZZA_FINESTRA = 800
DIMENSIONE_CELLA = 100
NUMERO_RIGHE = 8
NUMERO_COLONNE = 8

# Definizione dei simboli per i vari elementi del labirinto
ENTRATA = "E"
USCITA = "U"
MURO = "#"
VUOTO = " "
GIOCATORE = "P"

# Funzione per creare un labirinto vuoto
def crea_labirinto():
    labirinto = [[VUOTO] * NUMERO_COLONNE for _ in range(NUMERO_RIGHE)]
    return labirinto

# Funzione per posizionare l'entrata e l'uscita nel labirinto
def posiziona_entrata_uscita(labirinto):
    # Posiziona l'entrata in alto a sinistra
    labirinto[0][4] = ENTRATA
    # Posiziona l'uscita in basso a destra
    labirinto[3][7] = USCITA

# Funzione per posizionare i muri casualmente nel labirinto
def posiziona_muri(labirinto):
    #muro(pos(1,6)). muro(pos(2,8)). muro(pos(3,8)). muro(pos(4,4)). muro(pos(4,5)). muro(pos(5,5)). muro(pos(6,2)). muro(pos(7,2)). muro(pos(7,6)). muro(pos(8,3)).
    labirinto[1-1][6-1] = MURO
    labirinto[2-1][8-1] = MURO
    labirinto[3-1][8-1] = MURO
    labirinto[4-1][4-1] = MURO
    labirinto[4-1][5-1] = MURO
    labirinto[5-1][5-1] = MURO
    labirinto[6-1][2-1] = MURO
    labirinto[7-1][2-1] = MURO
    labirinto[7-1][6-1] = MURO
    labirinto[8-1][3-1] = MURO
# Funzione per inizializzare la posizione del giocatore
def inizializza_giocatore(labirinto):
    labirinto[1-1][5-1] = GIOCATORE

# Funzione per disegnare il labirinto
def disegna_labirinto(schermo, labirinto):
    for riga in range(NUMERO_RIGHE):
        for colonna in range(NUMERO_COLONNE):
            colore = NERO
            if labirinto[riga][colonna] == MURO:
                colore = BIANCO
            elif labirinto[riga][colonna] == ENTRATA:
                colore = VERDE
            elif labirinto[riga][colonna] == USCITA:
                colore = ROSSO
            elif labirinto[riga][colonna] == GIOCATORE:
                colore = GIALLO
            pygame.draw.rect(schermo, colore, (colonna * DIMENSIONE_CELLA, riga * DIMENSIONE_CELLA, DIMENSIONE_CELLA, DIMENSIONE_CELLA))

# Funzione per muovere il giocatore nel labirinto
def muovi_giocatore(labirinto, direzione):
    for riga in range(NUMERO_RIGHE):
        for colonna in range(NUMERO_COLONNE):
            if labirinto[riga][colonna] == GIOCATORE:
                if direzione == "su" and riga > 0 and labirinto[riga - 1][colonna] != MURO:
                    labirinto[riga][colonna] = VUOTO
                    labirinto[riga - 1][colonna] = GIOCATORE
                elif direzione == "giù" and riga < NUMERO_RIGHE - 1 and labirinto[riga + 1][colonna] != MURO:
                    labirinto[riga][colonna] = VUOTO
                    labirinto[riga + 1][colonna] = GIOCATORE
                elif direzione == "sinistra" and colonna > 0 and labirinto[riga][colonna - 1] != MURO:
                    labirinto[riga][colonna] = VUOTO
                    labirinto[riga][colonna - 1] = GIOCATORE
                elif direzione == "destra" and colonna < NUMERO_COLONNE - 1 and labirinto[riga][colonna + 1] != MURO:
                    labirinto[riga][colonna] = VUOTO
                    labirinto[riga][colonna + 1] = GIOCATORE

# Funzione per controllare se il giocatore ha raggiunto l'uscita
def controlla_vittoria(labirinto):
    for riga in labirinto:
        if GIOCATORE in riga:
            return False
    return True

# Funzione principale per eseguire il gioco
def gioca_labirinto():
    pygame.init()
    schermo = pygame.display.set_mode((LARGHEZZA_FINESTRA, ALTEZZA_FINESTRA))
    pygame.display.set_caption("Labirinto")
    clock = pygame.time.Clock()

    labirinto = crea_labirinto()
    posiziona_entrata_uscita(labirinto)
    posiziona_muri(labirinto)
    inizializza_giocatore(labirinto)

    while True:
        schermo.fill(NERO)
        disegna_labirinto(schermo, labirinto)
        pygame.display.update()
        clock.tick(30)

        for evento in pygame.event.get():
            if evento.type == pygame.QUIT:
                pygame.quit()
                sys.exit()
            elif evento.type == pygame.KEYDOWN:
                if evento.key == pygame.K_UP:
                    muovi_giocatore(labirinto, "su")
                elif evento.key == pygame.K_DOWN:
                    muovi_giocatore(labirinto, "giù")
                elif evento.key == pygame.K_LEFT:
                    muovi_giocatore(labirinto, "sinistra")
                elif evento.key == pygame.K_RIGHT:
                    muovi_giocatore(labirinto, "destra")

        if controlla_vittoria(labirinto):
            print("Hai vinto!")
            break

# Esegui il gioco
gioca_labirinto()
