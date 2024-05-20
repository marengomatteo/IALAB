:- dynamic g_cost/2.
:- dynamic parent/3.

euristica(pos(X, Y),[pos(XG1,YG1)],H) :- 
    H is (abs(X - XG1) + abs(Y - YG1)).

euristica(pos(X, Y), [pos(XG1, YG1) | Rest], H) :-
    H1 is abs(X - XG1) + abs(Y - YG1),
    euristica(pos(X, Y), Rest, HRest),
    H is min(H1, HRest).


/* f(n) = g(n) + h(n)*/
f_valutazione(Node, ListaFinali, F) :-
    g_cost(Node, G), 
    euristica(Node, ListaFinali, H), 
    F is G + H.

ricerca(Cammino,Cost) :-
    iniziale(S0),
    findall(Goal,finale(Goal),ListaFinali),
    retractall(g_cost(_, _)),
    retractall(parent(_, _,_)),
    assertz(g_cost(S0, 0)),
    a_star(ListaFinali,CamminoInverso,Cost,[S0],[]),
    inverti(CamminoInverso,Cammino).


a_star(ListaFinali,Cammino,Costo,StatiOpen,StatiClose) :-
    seleziona_nodo(StatiOpen,ListaFinali,pos(X,Y)),
    (
        member(pos(X,Y), ListaFinali) ->
        ( 
            reconstruct_path(pos(X,Y), Cammino,Costo), 
            g_cost(pos(X,Y), Costo)
        )
    ;
        (successori(pos(X,Y), Successori), % genera i successori
        subtract(StatiOpen,[pos(X,Y)],NewStatiOpen),
        append([pos(X,Y)],StatiClose,NewStatiClose),
        update_open_list(Successori, pos(X,Y),NewStatiOpen,NewStatiClose,RestOpened),% aggiorna la lista degli stati aperti
        a_star(ListaFinali, Cammino,Costo,RestOpened,NewStatiClose))
    ).

reconstruct_path(Node, _, _) :- % Caso base: nessun genitore, nessuna azione
    \+ parent(Node, _, _), !.
reconstruct_path(Node, [(Azione) | Path], Cost) :- % Caso ricorsivo: ricostruisce il percorso
    parent(Node, Parent, Azione),
    reconstruct_path(Parent, Path, Cost).

% Seleziona il miglior nodo dalla lista aperta
seleziona_nodo([N], _, N) :- !.
seleziona_nodo([N1, N2 | Rest], ListaFinali,  Best) :-
    f_valutazione(N1, ListaFinali, F1), % calcoliamo la funzione di valutazione sul primo nodo 
    f_valutazione(N2, ListaFinali, F2), % calcoliamo la funzione di valutazione sul secondo nodo
    F1 =< F2, % controlliamo il minore
    seleziona_nodo([N1 | Rest], ListaFinali, Best). %  se N1 minore di N2 allora richiamiamo seleziona nodo per controllare i nodi successivi

seleziona_nodo([_, N2 | Rest], ListaFinali, Best) :-
    seleziona_nodo([N2 | Rest], ListaFinali, Best).

/* genera tutti i successori*/
successori(S, Successori) :-
    findall([SNuovo,Azione],
            (applicabile(Azione, S),
             trasforma(Azione, S, SNuovo)),
            Successori).
    
retracta_tutto(Succ,GOld,GNew,Current,Azione) :-
     retract(g_cost(Succ, GOld)),
     retract(parent(Succ, _,_)),
     assertz(g_cost(Succ, GNew)),
     assertz(parent(Succ, Current,Azione)).

/* aggiorna la lista degli stati open */
update_open_list([],_,Aperti,_,Aperti).
update_open_list([[Succ,Azione] | Rest], Current, Aperti,Chiusi,RestOpen) :-
    ( 
        member(Succ,Chiusi) -> 
        update_open_list(Rest, Current,Aperti,Chiusi,RestOpen)
        ; (  g_cost(Current, GCurrent),
            GNew is GCurrent + 1, % il costo aumenta di 1
            (   member(Succ, Aperti)  -> 
                (
                	
                    g_cost(Succ, GOld), 
                    (   GNew < GOld ->   retracta_tutto(Succ,GOld,GNew,Current,Azione); true ),
                    update_open_list(Rest, Current,Aperti,Chiusi,RestOpen)
                )
                ;   
                (   
                    append([Succ],Aperti,NewApertiList),
                    assertz(g_cost(Succ, GNew)),
                    assertz(parent(Succ, Current,Azione)),
                    update_open_list(Rest, Current,NewApertiList,Chiusi,RestOpen)
                )
        	)
         )
    ).

inverti(L,LInv):-inv(L,[],LInv).
inv([],Temp,Temp).
inv([Head|Tail],Temp,LInv):-inv(Tail,[Head|Temp],LInv).