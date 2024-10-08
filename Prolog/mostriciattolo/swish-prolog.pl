muro(pos(1,6)).
muro(pos(2,2)).
muro(pos(2,8)).
muro(pos(3,8)).
muro(pos(4,4)).
muro(pos(4,5)).
muro(pos(5,5)).
muro(pos(6,2)).
muro(pos(7,1)).
muro(pos(7,2)).
muro(pos(7,6)).
muro(pos(7,7)).
muro(pos(7,8)).
muro(pos(8,3)).

ghiaccio(pos(2,6)).
ghiaccio(pos(2,7)).

mostriciattolo(pos(1,4)).

martello(pos(3,5)).

gemma(pos(8,4)).
gemma(pos(8,8)).
gemma(pos(5,4)).

finale(pos(4,8)).

cattivo(pos(8,1)).

:- dynamic incontrato/1.

/* ricerca */
ricerca(Cammino,Profondita,Step,Soglia, Bonus):-
    mostriciattolo(M),
    findall([gemma, G], gemma(G), PosGemme),
    findall(Gh, ghiaccio(Gh), PosGhiaccio),
    cattivo(C),
    assertz(incontrato(false)),
    ListaPos = [[mostro,M], [cattivo,C] | PosGemme ],
    itdeep(Profondita,Soglia,Step,Cammino, ListaPos, _, PosGhiaccio, Bonus).

/* iterative deepening */
itdeep(Profondita,Soglia,_,Cammino,ListaPos,HaMartello, PosGhiaccio, Bonus) :-
    Profondita =< Soglia,
    ric_prof(Profondita,[],Cammino,ListaPos,HaMartello, PosGhiaccio, Bonus).

itdeep(Profondita,Soglia,Step,Cammino, ListaPos,HaMartello, PosGhiaccio, Bonus) :-
    NuovaProf is Profondita + Step,
    NuovaProf =< Soglia,
    itdeep(NuovaProf,Soglia,Step,Cammino, ListaPos,HaMartello, PosGhiaccio, Bonus).

/* ricerca in profondità limitata */

ric_prof(_, _, [], [[_,Mostro],_|Tail],_, _, Bonus) :- 
    finale(Mostro),
    contigue_due_a_due(Tail, Bonus),
    !.

ric_prof(Profondita,AzEff,[Az|SeqAzioni],PosEl,HaMartello, PosGhiaccio, Bonus) :-
    Profondita > 0,
    nonRipetere(Az,AzEff),
    (   rovesciamento(Az,PosEl,PosRes, HaMartello, PosGhiaccio, NewPosGhiaccio) -> 
    	NuovaProfondita is Profondita-1,
    	append([Az],AzEff,NewAz),
        ric_prof(NuovaProfondita,NewAz,SeqAzioni,PosRes,HaMartello, NewPosGhiaccio, Bonus)
    	;
	    false ).


nonRipetere(est, [LastAz|_]) :- est \= LastAz.
nonRipetere(est, []).
nonRipetere(sud, [LastAz|_]) :- sud \= LastAz.
nonRipetere(sud, []).
nonRipetere(ovest, [LastAz|_]) :- ovest \= LastAz.
nonRipetere(ovest, []).
nonRipetere(nord, [LastAz|_]) :- nord \= LastAz.
nonRipetere(nord,[]).

rovesciamento(est,PosElementi,ListaNew,HaMartello, PosGhiaccio, NewPosGhiaccio) :-
    righe_elementi(est,PosElementi, Lista),
    itera_righe(est,Lista, PosElementi,ListaNew,HaMartello, PosGhiaccio, NewPosGhiaccio),!,
    ( incontrato(X), X == true ->
    	retract(incontrato(X)),
    	assertz(incontrato(false)), 
    	fail; 
    	true
    ).

rovesciamento(ovest,PosElementi,ListaNew,HaMartello, PosGhiaccio, NewPosGhiaccio) :-
    righe_elementi(ovest,PosElementi, Lista),
    itera_righe(ovest,Lista, PosElementi,ListaNew,HaMartello, PosGhiaccio, NewPosGhiaccio),!,
     ( incontrato(X), X == true -> retract(incontrato(X)),
    assertz(incontrato(false)), fail; true).

rovesciamento(nord,PosElementi,ListaNew,HaMartello, PosGhiaccio, NewPosGhiaccio) :-
    colonne_elementi(nord,PosElementi, Lista),
	itera_colonne(nord,Lista, PosElementi,ListaNew,HaMartello, PosGhiaccio, NewPosGhiaccio),!,
    ( incontrato(X), X == true -> retract(incontrato(X)),assertz(incontrato(false)), fail
    ; true).

rovesciamento(sud,PosElementi,ListaNew,HaMartello, PosGhiaccio, NewPosGhiaccio) :-
    colonne_elementi(sud,PosElementi, Lista),
	itera_colonne(sud,Lista, PosElementi,ListaNew,HaMartello, PosGhiaccio, NewPosGhiaccio),!,
    ( incontrato(X), X == true -> retract(incontrato(X)),
    assertz(incontrato(false)), fail; true).


righe_elementi(_,[], []). 
righe_elementi(Az,[[_,pos(R, _)] | T], ListaRighe) :- 
    righe_elementi(Az,T, RT),
    sort([R | RT],ListaRighe).

colonne_elementi(_,[], []). 
colonne_elementi(Az,[[_,pos(_, C)] | T], ListaColonne) :- 
    colonne_elementi(Az,T, CT),
    sort([C | CT],ListaColonne). 

itera_righe(_,[], ListaNew,ListaNew,_, PosGhiaccio, PosGhiaccio).
% head: prima riga, tail righe dopo
itera_righe(Az,[Head|Tail], PosElementi, ListaNew,HaMartello, PosGhiaccio, NewPosGhiaccio) :-
    ordina_per_colonna(Az,Head,PosElementi,Res),
    controllo_oggetti(Az,Res,PosElementi,NewPosElementi,HaMartello, PosGhiaccio, NewPosGhiaccio),
    itera_righe(Az,Tail,NewPosElementi, ListaNew,HaMartello, PosGhiaccio, NewPosGhiaccio).

itera_colonne(_,[], ListaNew,ListaNew,_, PosGhiaccio, PosGhiaccio).
% head: prima riga, tail righe dopo
itera_colonne(Az,[Head|Tail], PosElementi, ListaNew,HaMartello, PosGhiaccio, NewPosGhiaccio) :-
    ordina_per_riga(Az,Head,PosElementi,Res),
    controllo_oggetti(Az,Res,PosElementi,NewPosElementi,HaMartello, PosGhiaccio, NewPosGhiaccio),
    itera_colonne(Az,Tail,NewPosElementi, ListaNew,HaMartello, PosGhiaccio, NewPosGhiaccio).

ordina_per_colonna(Az, Riga, ListaPosizioni, ListaOrdinata) :-
    trova_elementi_riga( Riga, ListaPosizioni, ListaFiltrata),
    (  Az = est ->  
    	sort(2, >, ListaFiltrata, ListaOrdinata);
    	sort(2, <, ListaFiltrata, ListaOrdinata)
    ).
ordina_per_riga(Az, Colonna, ListaPosizioni, ListaOrdinata) :-
    trova_elementi_colonna( Colonna, ListaPosizioni, ListaFiltrata),
    (  Az = sud ->  
    	sort(2, >, ListaFiltrata, ListaOrdinata); 
    	sort(2, <, ListaFiltrata, ListaOrdinata)
    ).

trova_elementi_riga( R,ListaPos, ElementiRiga) :-
    include(ha_riga(R), ListaPos, ElementiRiga).
ha_riga(R, [_,pos(R, _)]).

trova_elementi_colonna( C,ListaPos, ElementiColonne) :-
    include(ha_colonna(C), ListaPos, ElementiColonne).
ha_colonna(C, [_,pos(_, C)]).

controllo_oggetti(Az,[[_,pos(R,C)] | Tail],ListaEl, NewListaPosTail,HaMartello, PosGhiaccio, NewPosGhiaccio) :-
    trova_elemento(pos(R,C), ListaEl, Tipo),
    posizione_valida(Az,Tipo, pos(R,C), pos(R1,C1), ListaEl,HaMartello, PosGhiaccio, NewPosG),
    modifica_posizione(ListaEl,pos(R,C),pos(R1,C1), NewListaPos),
    controllo_oggetti(Az,Tail,NewListaPos, NewListaPosTail,HaMartello, NewPosG, NewPosGhiaccio).
controllo_oggetti(_, [], NewListaPos, NewListaPos,_, PosGhiaccio, PosGhiaccio).

/* --- posizione valida EST --- */
posizione_valida(est,Tipo,pos(R,C),pos(R,C1),ListaPos,HaMartello,PosGhiaccio, NewPosG) :-
    C2 is C+1,C2 =< 8,
     ( ( Tipo = mostro; Tipo = cattivo) ->  incontra_cattivo(Tipo, pos(R,C2),ListaPos); true  ),
     ( Tipo = mostro -> check_martello(pos(R,C2),HaMartello);true),
    \+ occupata(pos(R,C2),Tipo, ListaPos,HaMartello,PosGhiaccio, NewPosG),
    posizione_valida(est,Tipo,pos(R,C2),pos(R,C1), ListaPos,HaMartello, PosGhiaccio, NewPosG).

posizione_valida(est,_,pos(R,C),pos(R,C),_,_,PosGhiaccio, PosGhiaccio).

/* --- posizione valida SUD --- */
posizione_valida(sud,Tipo, pos(R,C), pos(R1,C), ListaPos,HaMartello,PosGhiaccio, NewPosG) :-
    R2 is R+1,R2 =< 8,
    ( ( Tipo = mostro; Tipo = cattivo) ->  incontra_cattivo(Tipo, pos(R2,C),ListaPos); true  ),
    (   Tipo = mostro -> check_martello(pos(R2,C),HaMartello);true),
    \+ occupata(pos(R2,C),Tipo, ListaPos,HaMartello,PosGhiaccio, NewPosG),
    posizione_valida(sud,Tipo, pos(R2,C), pos(R1,C), ListaPos,HaMartello,PosGhiaccio, NewPosG).

posizione_valida(sud,_,pos(R,C),pos(R,C),_,_,PosGhiaccio, PosGhiaccio).
    
/* --- posizione valida OVEST --- */
posizione_valida(ovest,Tipo,pos(R,C),pos(R,C1),ListaPos,HaMartello,PosGhiaccio, NewPosG) :-
    C2 is C-1,C2 >= 1,
    ( ( Tipo = mostro; Tipo = cattivo) ->  incontra_cattivo(Tipo, pos(R,C2),ListaPos); true  ),
    (   Tipo = mostro -> check_martello(pos(R,C2),HaMartello) ; true),
    \+ occupata(pos(R,C2), Tipo,ListaPos,HaMartello,PosGhiaccio, NewPosG),
    posizione_valida(ovest,Tipo, pos(R,C2),pos(R,C1), ListaPos,HaMartello,PosGhiaccio, NewPosG).

posizione_valida(ovest,_, pos(R,C), pos(R, C),_,_,PosGhiaccio, PosGhiaccio).
   
/* --- posizione valida NORD --- */
posizione_valida(nord,Tipo, pos(R,C), pos(R1,C), ListaPos,HaMartello,PosGhiaccio, NewPosG) :-
    R2 is R-1,  R2 >= 1,
    ( ( Tipo = mostro; Tipo = cattivo) ->  incontra_cattivo(Tipo, pos(R2,C),ListaPos); true  ),
    (   Tipo = mostro -> check_martello(pos(R2,C),HaMartello) ; true ),
    \+ occupata(pos(R2,C),Tipo, ListaPos,HaMartello,PosGhiaccio, NewPosG),
    posizione_valida(nord,Tipo, pos(R2,C), pos(R1,C), ListaPos,HaMartello,PosGhiaccio, NewPosG).
posizione_valida(nord,_,pos(R,C),pos(R,C),_,_,PosGhiaccio, PosGhiaccio).

incontra_cattivo(Tipo,Pos,[[_,Mostro],[_,Cattivo]|_]) :-
    ( Tipo = mostro,Pos == Cattivo -> retract(incontrato(_)),assertz(incontrato(true)));
    ( Tipo = cattivo,Pos == Mostro -> retract(incontrato(_)),assertz(incontrato(true))).
incontra_cattivo(_,_,_).

check_martello(pos(R,C), HaMartello):- ha_martello(pos(R,C), HaMartello).

occupata(pos(R,C),Tipo,ListaEl,HaMartello,PosGhiaccio,NewPosG) :-
 	member([_,pos(R, C)], ListaEl);
    (   
       muro(pos(R,C));
       ( ( Tipo = mostro, HaMartello == true, member(pos(R,C),PosGhiaccio)) ->   
              elimina_elemento(pos(R,C), PosGhiaccio, NewPosG),
              false
               ;
               member(pos(R,C),PosGhiaccio),
              NewPosG = PosGhiaccio
       )).


elimina_elemento(pos(R,C), [pos(R,C)|T], T) :- !.
elimina_elemento(Posizione, [H|T], [H|R]) :-
    elimina_elemento(Posizione, T, R).
elimina_elemento(_, [], []).

ha_martello(pos(R,C), true) :- martello(pos(R, C)).
ha_martello(_, _).

trova_elemento(pos(R,C), [[Tipo, pos(R,C)]|_], Tipo) :- !.
trova_elemento(Posizione, [_|T], Elemento) :-
    trova_elemento(Posizione, T, Elemento).

modifica_posizione([], _,_, []).

modifica_posizione([[Tipo,pos(R,C)] | Resto], CurrentPos,NewPos, [[Tipo,ElPos]| NuovoResto]) :-
    (   pos(R,C) = CurrentPos -> ElPos = NewPos; ElPos = pos(R,C) ),
    modifica_posizione(Resto, CurrentPos, NewPos, NuovoResto).

adiacenti(pos(R1,C1), pos(R2,C2)) :-
    (R1 =:= R2, abs(C1 - C2) =:= 1);
    (C1 =:= C2, abs(R1 - R2) =:= 1).

contigue_due_a_due(ListaEl, Bonus) :-
    findall(pos(R, C), member([gemma, pos(R, C)], ListaEl), [Pos1, Pos2, Pos3]),
    ha_bonus([Pos1, Pos2,Pos3], Bonus).
    
ha_bonus([Pos1,Pos2,Pos3], true) :- 
    (   (adiacenti(Pos1, Pos2), adiacenti(Pos2, Pos3));
        (adiacenti(Pos1, Pos3), adiacenti(Pos3, Pos2));
    	(adiacenti(Pos1, Pos2), adiacenti(Pos3, Pos1))
    ).
ha_bonus(_, false).
