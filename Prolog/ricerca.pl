ricerca(Cammino):-
    iniziale(S0),
    ric_prof(S0,[],Cammino).

ric_prof(S,_,[]):-finale(S),!.
ric_prof(S,Visitati,[Az|SeqAzioni]):-
    applicabile(Az,S),
    trasforma(Az,S,SNuovo),
    \+member(SNuovo,Visitati),
    ric_prof(SNuovo,[S|Visitati],SeqAzioni).
