# Progetto IA lab gruppo: Frattarola, Giacolono, Marengo

## PROLOG

### labirinto
```shell script
working_directory(_, "PATH/IALAB/Prolog/labirinto").
['labirinto.pl'].
['azioni.pl'].
```

#### iterative-deepening
```shell script
['iterative-deepening.pl'].
ricerca(Sol,5,5,30),write(Sol).
```

#### a-star
```shell script
['euristica.pl'].
ricerca(Sol,Costo),write(Sol),write(Costo).
```

### mostriciattolo
```shell script
working_directory(_, "PATH/IALAB/Prolog/mostriciattolo").
['mostriciattolo.pl'].
['azioni.pl'].
['iterative-deepening.pl'].
ricerca(Sol,10,5,30,Bonus),write(Sol),write(Bonus).
```

## ASP
```shell script
clingo Calendario.lp
```


## CLIPS
### MAC
```shell script
(batch "PATH/IALAB/CLIPS/master-mind/load.sh")
```

### WINDOWS
```shell script
(batch C:/Users/marco/OneDrive/Desktop/IALAB/IALAB-1/CLIPS/master-mind/go.bat)
```
