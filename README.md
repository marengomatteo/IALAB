Progetto IA lab gruppo: Frattarola, Giacolono, Marengo


Per tutti voi:
prima di ogni volta che usate prolog dovete lanciare questo comando settando come working directory la directory corrente
working_directory(_, "PATH_DOVE_AVETE_LE_VOSTRE_BELLISIME_COSE").

['labirinto.pl'].
['azioni.pl'].
['prof_lim.pl'].
ricerca(Sol,20),write(Sol).




Per clips:
(chdir "PATH")
(load "file.name")



TODO: Provare a modificare lo spostamento del mostriciatttolo. al posto di spostare una alla volta controlliamo finchè non trova caselle vuote e alla prima occupata si sposta nelle coordinate dell'occupata -1. Per le gemme possiamo tenere una regola che se incontra una gemma prosegue e imposta la gemma a -1 e il mostro a -2 :)