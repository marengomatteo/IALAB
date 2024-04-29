ricerca(Cammino,Soglia):-
    iniziale(S0),
    ric_prof(S0,Soglia,[],Cammino).

ric_prof(S,_,_,[]):-finale(S),!.
ric_prof(S,Soglia,Visitati,[Az|SeqAzioni]):-
    Soglia > 0,
    applicabile(Az,S),
    trasforma(Az,S,SNuovo),
    \+member(SNuovo,Visitati),
    NuovaSoglia is Soglia-1,
    ric_prof(SNuovo,NuovaSoglia,[S|Visitati],SeqAzioni).
