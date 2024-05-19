
:- dynamic g_cost/2.
:- dynamic parent/2.



euristica(pos(X, Y), pos(XG, YG), H) :-
    H is (abs(X - XG) + abs(Y - YG)).

/* f(n) = g(n) + h(n)*/
f_valutazione(Node, Goal, F) :-
    g_cost(Node, G), 
    euristica(Node, Goal, H), 
    F is G + H.

ricerca(Cammino,Cost) :-
    iniziale(S0),
    finale(Goal),
    retractall(g_cost(_, _)),
    retractall(parent(_, _)),
    assertz(g_cost(S0, 0)),
    a_star(Goal,CamminoInverso,Cost,[S0],[]),
    inverti(CamminoInverso,Cammino).


a_star(Goal,Cammino,Costo,StatiOpen,StatiClose) :-
    seleziona_nodo(StatiOpen, Goal,Current),
    (
        Current = Goal ->
        (reconstruct_path(Goal, Cammino,Costo), 
        g_cost(Goal, Costo))
    ;
        (successori(Current, Successori), % genera i successori
        subtract(StatiOpen,[Current],NewStatiOpen),
        append([Current],StatiClose,NewStatiClose),
        update_open_list(Successori, Current, Goal,NewStatiOpen,NewStatiClose,RestOpened),% aggiorna la lista degli stati aperti
        a_star(Goal, Cammino,Costo,RestOpened,NewStatiClose))
    ).

reconstruct_path(Node, [Node], _) :-
    \+ parent(Node, _), !.
reconstruct_path(Node, [Node | Path], Cost) :-
    parent(Node, Parent),
    reconstruct_path(Parent, Path, Cost).

% Seleziona il miglior nodo dalla lista aperta
% ha un problema ma risolvibile oppure ignorabile
seleziona_nodo([N], _, N) :- !.
seleziona_nodo([N1, N2 | Rest], Goal,  Best) :-
    f_valutazione(N1, Goal, F1), % calcoliamo la funzione di valutazione sul primo nodo 
    f_valutazione(N2, Goal, F2), % calcoliamo la funzione di valutazione sul secondo nodo
    F1 =< F2, % controlliamo il minore
    seleziona_nodo([N1 | Rest], Goal, Best). % probabile cut qui, se N1 minore di N2 allora richiamiamo seleziona nodo per controllare i nodi successivi

seleziona_nodo([_, N2 | Rest], Goal, Best) :-
    seleziona_nodo([N2 | Rest], Goal, Best).

/* genera tutti i successori*/
successori(S, Successori) :-
    findall(SNuovo,
            (applicabile(Azione, S),
             trasforma(Azione, S, SNuovo)),
            Successori).
    
retracta_tutto(Succ,GOld,GNew,Current) :-
     retract(g_cost(Succ, GOld)),
     retract(parent(Succ, _)),
     assertz(g_cost(Succ, GNew)),
     assertz(parent(Succ, Current)).

/* aggiorna la lista degli stati open */
update_open_list([],_,_,Aperti,_,Aperti).
update_open_list([Succ | Rest], Current, Goal, Aperti,Chiusi,RestOpen) :-
    ( 
        member(Succ,Chiusi) -> 
        update_open_list(Rest, Current, Goal,Aperti,Chiusi,RestOpen)
        ; (  g_cost(Current, GCurrent),
            GNew is GCurrent + 1, % il costo aumenta di 1
            (   member(Succ, Aperti)  -> 
                (
                	
                    g_cost(Succ, GOld), 
                    (   GNew < GOld ->   retracta_tutto(Succ,GOld,GNew,Current); true ),
                    update_open_list(Rest, Current, Goal,Aperti,Chiusi,RestOpen)
                )
                ;   
                (   
                    append([Succ],Aperti,NewApertiList),
                    assertz(g_cost(Succ, GNew)),
                    assertz(parent(Succ, Current)),
                    update_open_list(Rest, Current, Goal,NewApertiList,Chiusi,RestOpen)
                )
        	)
         )
    ).

inverti(L,LInv):-inv(L,[],LInv).
inv([],Temp,Temp).
inv([Head|Tail],Temp,LInv):-inv(Tail,[Head|Temp],LInv).