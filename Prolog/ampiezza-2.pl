ricerca(Cammino) :-
    iniziale(S0), % stato iniziale 
    ampiezza([S0,[]], Cammino). 
/**
 	[S0]: rappresenta una lista contenente S0, lista degli stati già incontrati ma non "espansi"
 **/

/**
 Lo stato S è finale quindi ho finito
**/
ampiezza([[S,Cammino]|_],[],Cammino) :- finale(S),!. 
/** 
 !: lo stato è finale, passo al successivo solo se S non è finale
**/

/**
 	* controlliamo che S non sia già stato visitato
	* generiamo i figli: findall tutte le azioni applicabili in quello stato
	* creiamo i figli: creiamo una lista di nuovi stati
 	* listaNuoviStati: li andiamo ad accodare in tail, se già ci sono non dobbiamo riaggiungerli
	* aggiungiamo i nuovi stati alla tail
    * invoco ampiezza togliendo S ma mantenendolo in memoria perché se dovesse ripresentarsi posso buttarlo via, il risultato  
**/
ampiezza([[S,Cammino]|Tail],Visitati,Risultato) :-
    \+member(S,Visitati),!,
    findall(Az,applicabile(Az,S), ListaAzioni),
    generaNuoviStati([S,Cammino],ListaAzioni,ListaNuoviStati),
    differenza(ListaNuoviStati,Tail,StatiDaAggiungere),
    append(Tail,StatiDaAggiungere,NuovaTail),
    ampiezza(NuovaTail,[S,Visitati],Risultato).

generaNuoviStati(_,[],[]).
generaNuoviStati([S,Cammino],[Az|Tail],
                 [[SNuovo,[Az|Cammino]],|ListaTail]) :-
    trasforma(Az,S,SNuovo),
    generaNuoviStati([S,Cammino],Tail,ListaTail).

differenza([],_,[]).
differenza([[S,_]|Tail],B,Risultato) :-
    member([S,_],B),!,
    differenza(Tail,B,Risultato).
differenza([[S,Cammino]|Tail],B,[[S,Cammino]|RisTail]) :-
    differenza(Tail,B,RisTail).


