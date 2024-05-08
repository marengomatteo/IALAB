import tkinter as tk
from tkinter import scrolledtext

def extract_number(record):
    # Estrae il numero dalla stringa tra le parentesi tonde dopo l'ultima virgola ","
    return int(record.split(",")[-1].split(")")[-2])

def sort_records():
    input_text = text_area.get("1.0", "end-1c")  # Ottieni il testo dall'area di testo
    # Dividi il testo in righe
    lines = input_text.split("\n")
    # Rimuovi eventuali righe vuote
    lines = [line.strip() for line in lines if line.strip()]

    # Ordina le righe in base al numero estratto dalla funzione extract_number
    sorted_lines = sorted(lines, key=extract_number)

    # Unisci le righe ordinate in una stringa
    sorted_text = "\n".join(sorted_lines)

    # Visualizza il testo ordinato nella seconda area di testo
    sorted_text_area.delete("1.0", tk.END)
    sorted_text_area.insert(tk.END, sorted_text)

# Creazione della finestra principale
window = tk.Tk()
window.title("Ordinatore di Testo")

# Etichetta e area di testo per il testo non ordinato
tk.Label(window, text="Incolla il testo da ordinare qui:").pack()
text_area = scrolledtext.ScrolledText(window, width=50, height=10)
text_area.pack()

# Pulsante per ordinare il testo
tk.Button(window, text="Ordina", command=sort_records).pack()

# Etichetta e area di testo per il testo ordinato
tk.Label(window, text="Testo ordinato:").pack()
sorted_text_area = scrolledtext.ScrolledText(window, width=50, height=10)
sorted_text_area.pack()

# Loop principale per l'interfaccia utente
window.mainloop()
