ricerca(Cammino):-
    iniziale(S0),
    ampiezza([[S0,[]]],[],CamminoInverso),
    inverti(CamminoInverso,Cammino).

ampiezza([[S,Cammino]|_],_,Cammino):-finale(S),!.
ampiezza([[S,Cammino]|Tail],Visitati,Risultato):-
    \+member(S,Visitati),!,
    findall(Az,applicabile(Az,S),ListaAzioni),
    generaNuoviStati([S,Cammino],ListaAzioni,ListaNuoviStati),
    differenza(ListaNuoviStati,Tail,StatiDaAggiungere),
    append(Tail,StatiDaAggiungere,NuovaTail),
    ampiezza(NuovaTail,[S|Visitati],Risultato).
ampiezza([_|Tail],Visitati,Risultato):-
    ampiezza(Tail,Visitati,Risultato).

generaNuoviStati(_,[],[]).
generaNuoviStati([S,Cammino],[Az|Tail],
       [[SNuovo,[Az|Cammino]]|ListaTail]):-
    trasforma(Az,S,SNuovo),
    generaNuoviStati([S,Cammino],Tail,ListaTail).
differenza([],_,[]).
differenza([[S,_]|Tail],B,Risultato):-
    member([S,_],B),!,
    differenza(Tail,B,Risultato).
differenza([[S,Cammino]|Tail],B,[[S,Cammino]|RisTail]):-
    differenza(Tail,B,RisTail).

inverti(L,LInv):-inv(L,[],LInv).
inv([],Temp,Temp).
inv([Head|Tail],Temp,LInv):-inv(Tail,[Head|Temp],LInv).

