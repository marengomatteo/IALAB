/* ricerca */
ricerca(Cammino,Profondita,Step,Soglia):-
    iniziale(S0),
    itdeep(S0,Profondita,Soglia,Step,Cammino).

/* iterative deepening */
itdeep(S,Profondita,Soglia,_,Cammino) :-
    Profondita =< Soglia,
    ric_prof(S,Profondita,[],Cammino).

itdeep(S,Profondita,Soglia,Step,Cammino) :-
    NuovaProf is Profondita + Step,
    itdeep(S,NuovaProf,Soglia,Step,Cammino).

/* ricerca in profondità limitata */

ric_prof(S,_,_,[]) :- finale(S),!.

ric_prof(S,Profondita,Visitati,[Az|SeqAzioni]):-
    Profondita > 0,
    nonRipetere(Az,SeqAzioni),
    trasforma(Az,S, SNuovo),
    NuovaProfondita is Profondita-1,
    ric_prof(SNuovo,NuovaProfondita,[S|Visitati],SeqAzioni).


