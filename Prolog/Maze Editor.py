import tkinter as tk

PATH_FRATTA = "C:/Users/marco/OneDrive/Desktop/maze.txt"
PATH_MATTE = "/Users/matteomarengo/Documents/uni/IALAB/Prolog"

class MazeEditor:
    def __init__(self, master):
        self.master = master
        self.master.title("Maze Editor")
        self.cell_size = 30
        self.canvas_width = 0
        self.canvas_height = 0
        self.start_point = None
        self.end_point = None
        self.walls = set()
        self.create_widgets()

    def create_widgets(self):
        self.label = tk.Label(self.master, text="Dimensioni del labirinto (righe x colonne):", font=("Helvetica", 12))
        self.label.grid(row=0, column=0, columnspan=2, pady=5)

        self.rows_entry = tk.Entry(self.master, font=("Helvetica", 12), width=5)
        self.rows_entry.grid(row=1, column=0, padx=5, pady=5)
        self.cols_entry = tk.Entry(self.master, font=("Helvetica", 12), width=5)
        self.cols_entry.grid(row=1, column=1, padx=5, pady=5)

        self.create_button = tk.Button(self.master, text="Crea Labirinto", command=self.create_maze)
        self.create_button.grid(row=2, column=0, columnspan=2, pady=5)

        self.save_button = tk.Button(self.master, text="Salva", command=self.save_maze)
        self.save_button.grid(row=4, column=0, columnspan=2, pady=5)

        self.canvas = tk.Canvas(self.master, bg="white")
        self.canvas.grid(row=3, column=0, columnspan=2)

    def create_maze(self):
        rows = int(self.rows_entry.get())
        cols = int(self.cols_entry.get())

        self.canvas.delete("all")

        self.canvas_width = cols * self.cell_size
        self.canvas_height = rows * self.cell_size
        self.canvas.config(width=self.canvas_width, height=self.canvas_height)

        for i in range(rows):
            for j in range(cols):
                # Aggiungi bordi colorati ai quadrati del labirinto
                self.canvas.create_rectangle(j * self.cell_size, i * self.cell_size,
                                             (j + 1) * self.cell_size, (i + 1) * self.cell_size,
                                             outline="#333", width=2)  # Aumenta lo spessore del bordo

        self.canvas.bind("<Button-1>", self.place_start_point)
        self.canvas.bind("<Button-3>", self.place_end_point)
        self.canvas.bind("<B1-Motion>", self.place_wall)
        self.canvas.bind("<B3-Motion>", self.remove_wall)

    def place_start_point(self, event):
        col = event.x // self.cell_size
        row = event.y // self.cell_size
        if self.start_point:
            self.canvas.delete(self.start_point_id)  # Elimina il punto di inizio precedente
        self.start_point = (col, row)
        self.start_point_id = self.canvas.create_oval(col * self.cell_size, row * self.cell_size,
                                                      (col + 1) * self.cell_size, (row + 1) * self.cell_size,
                                                      fill="green")

    def place_end_point(self, event):
        col = event.x // self.cell_size
        row = event.y // self.cell_size
        if self.end_point:
            self.canvas.delete(self.end_point_id)  # Elimina il punto di fine precedente
        self.end_point = (col, row)
        self.end_point_id = self.canvas.create_oval(col * self.cell_size, row * self.cell_size,
                                                    (col + 1) * self.cell_size, (row + 1) * self.cell_size,
                                                    fill="red")



    def place_wall(self, event):
        col = event.x // self.cell_size
        row = event.y // self.cell_size
        wall_id = "wall_{}_{}".format(row, col)
        if (row, col) not in self.walls:
            self.walls.add((row, col))
            self.canvas.create_rectangle(col * self.cell_size, row * self.cell_size,
                                         (col + 1) * self.cell_size, (row + 1) * self.cell_size,
                                         fill="black", tags=wall_id)

    def remove_wall(self, event):
        col = event.x // self.cell_size
        row = event.y // self.cell_size
        if (row, col) in self.walls:
            self.walls.remove((row, col))
            self.canvas.delete("wall_{}_{}".format(row, col))

    def save_maze(self):
        rows = int(self.rows_entry.get())
        cols = int(self.cols_entry.get())
        start_pos = self.start_point if self.start_point else None
        end_pos = self.end_point if self.end_point else None
        with open(PATH_MATTE, "w") as f:
            f.write(f'num_righe({rows}).\n')
            f.write(f'num_col({cols}).\n')
            if start_pos:
                f.write(f'iniziale(pos({start_pos[1]+1},{start_pos[0]+1})).\n')
            if end_pos:
                f.write(f'finale(pos({end_pos[1]+1},{end_pos[0]+1})).\n')
            for wall in self.walls:
               f.write(f'occupata(pos({wall[0]+1},{wall[1]+1})).\n')




def main():
    root = tk.Tk()
    maze_editor = MazeEditor(root)
    root.mainloop()

if __name__ == "__main__":
    main()
