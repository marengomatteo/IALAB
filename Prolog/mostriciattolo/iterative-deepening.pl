/* ricerca */
ricerca(Cammino,Profondita,Step,Soglia):-
    itdeep(Profondita,Soglia,Step,Cammino).

/* iterative deepening */
itdeep(Profondita,Soglia,_,Cammino) :-
    Profondita =< Soglia,
    ric_prof(Profondita,[],Cammino).

itdeep(Profondita,Soglia,Step,Cammino) :-
    NuovaProf is Profondita + Step,
    itdeep(NuovaProf,Soglia,Step,Cammino).

/* ricerca in profondità limitata */

ric_prof(_,_,[]) :- 
    mostriciattolo(G),
    finale(G), !.

ric_prof(Profondita,Visitati,[Az|SeqAzioni]) :-
    trace,
    Profondita > 0,
    rovesciamento(Az),
    NuovaProfondita is Profondita-1,
    ric_prof(NuovaProfondita,[Visitati],SeqAzioni).


