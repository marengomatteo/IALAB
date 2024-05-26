muro(pos(1,6)).
muro(pos(2,2)).
muro(pos(2,8)).
muro(pos(3,8)).
muro(pos(4,4)).
muro(pos(4,5)).
muro(pos(5,5)).
muro(pos(6,2)).
muro(pos(7,2)).
muro(pos(7,6)).
muro(pos(8,3)).

ghiaccio(pos(2,6)).
ghiaccio(pos(2,7)).
ghiaccio(pos(7,7)).

mostriciattolo(pos(1,5)).

martello(pos(3,5)).

gemma(pos(1,7)).
gemma(pos(2,3)).
gemma(pos(7,3)).

finale(pos(4,8)).

/* ricerca */
ricerca(Cammino,Profondita,Step,Soglia):-
    mostriciattolo(M),
    findall([gemma, G], gemma(G), PosGemme),
    ListaPos = [[mostro,M] | PosGemme],
    itdeep(Profondita,Soglia,Step,Cammino, ListaPos).

/* iterative deepening */
itdeep(Profondita,Soglia,_,Cammino,ListaPos) :-
    Profondita =< Soglia,
    ric_prof(Profondita,[],Cammino,ListaPos,_).

itdeep(Profondita,Soglia,Step,Cammino, ListaPos) :-
    NuovaProf is Profondita + Step,
    itdeep(NuovaProf,Soglia,Step,Cammino, ListaPos).

/* ricerca in profondità limitata */

ric_prof(_, _, [], [[_,Mostro]|_], _) :- 
    finale(Mostro), 
    !.

ric_prof(Profondita,AzEff,[Az|SeqAzioni],PosEl,_) :-
    Profondita > 0,
    nonRipetere(Az,AzEff),
    ( rovesciamento(Az,PosEl,PosRes) ->  
    	NuovaProfondita is Profondita-1,
    	append([Az],AzEff,NewAz),
        ric_prof(NuovaProfondita,NewAz,SeqAzioni,PosRes,_)
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

rovesciamento(est,PosElementi,ListaNew) :-
    righe_elementi(est,PosElementi, Lista),
    itera_righe(est,Lista, PosElementi,ListaNew).

rovesciamento(ovest,PosElementi,ListaNew) :-
    righe_elementi(ovest,PosElementi, Lista),
    itera_righe(ovest,Lista, PosElementi,ListaNew).

rovesciamento(nord,PosElementi,ListaNew) :-
    colonne_elementi(nord,PosElementi, Lista),
	itera_colonne(nord,Lista, PosElementi,ListaNew).

rovesciamento(sud,PosElementi,ListaNew) :-
    colonne_elementi(sud,PosElementi, Lista),
	itera_colonne(sud,Lista, PosElementi,ListaNew).

righe_elementi(_,[], []). 
righe_elementi(Az,[[_,pos(R, _)] | T], ListaRighe) :- 
    righe_elementi(Az,T, RT),
    sort([R | RT],ListaRighe). 

colonne_elementi(_,[], []). 
colonne_elementi(Az,[[_,pos(_, C)] | T], ListaColonne) :- 
    colonne_elementi(Az,T, CT),
    sort([C | CT],ListaColonne). 

itera_righe(_,[], ListaNew,ListaNew).
% head: prima riga, tail righe dopo
itera_righe(Az,[Head|Tail], PosElementi, ListaNew) :-
    ordina_per_colonna(Az,Head,PosElementi,Res),
    controllo_oggetti(Az,Res,PosElementi,NewPosElementi),
    itera_righe(Az,Tail,NewPosElementi, ListaNew).

itera_colonne(_,[], ListaNew,ListaNew).
% head: prima riga, tail righe dopo
itera_colonne(Az,[Head|Tail], PosElementi, ListaNew) :-
    ordina_per_riga(Az,Head,PosElementi,Res),
    controllo_oggetti(Az,Res,PosElementi,NewPosElementi),
    itera_colonne(Az,Tail,NewPosElementi, ListaNew).


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

trova_elementi_colonna( C,ListaPos, ElementiColonne) :-
    include(ha_colonna(C), ListaPos, ElementiColonne).
ha_colonna(C, [_,pos(_, C)]).

trova_elementi_riga( R,ListaPos, ElementiRiga) :-
    include(ha_riga(R), ListaPos, ElementiRiga).
ha_riga(R, [_,pos(R, _)]).

controllo_oggetti(Az,[[_,pos(R,C)] | Tail],ListaEl, NewListaPosTail) :-
    posizione_valida(Az, pos(R,C), pos(R1,C1), ListaEl),
    modifica_posizione(ListaEl,pos(R,C),pos(R1,C1), NewListaPos),
    controllo_oggetti(Az,Tail,NewListaPos, NewListaPosTail).
controllo_oggetti(_, [], NewListaPos, NewListaPos).

/* --- posizione valida EST --- */
posizione_valida(est,pos(R,C),pos(R,C1), ListaPos) :-
    C2 is C+1,C2 =< 8,
    \+ occupata(pos(R,C2), ListaPos),
    posizione_valida(est, pos(R,C2),pos(R,C1), ListaPos).
posizione_valida(est, pos(R,C), pos(R, C), _).

/* --- posizione valida SUD --- */
posizione_valida(sud, pos(R,C), pos(R1,C), ListaPos) :-
    R2 is R+1,R2 =< 8,
    \+ occupata(pos(R2,C), ListaPos),
    posizione_valida(sud, pos(R2,C), pos(R1,C), ListaPos).
posizione_valida(sud, pos(R,C), pos(R,C),_).

/* --- posizione valida OVEST --- */
posizione_valida(ovest,pos(R,C),pos(R,C1), ListaPos) :-
    C2 is C-1,C2 >= 1,
    \+ occupata(pos(R,C2), ListaPos),
    posizione_valida(ovest, pos(R,C2),pos(R,C1), ListaPos).
posizione_valida(ovest, pos(R,C), pos(R, C), _).

/* --- posizione valida NORD --- */
posizione_valida(nord, pos(R,C), pos(R1,C), ListaPos) :-
    R2 is R-1,  R2 >= 1,
    \+ occupata(pos(R2,C), ListaPos),
    posizione_valida(nord, pos(R2,C), pos(R1,C), ListaPos).
posizione_valida(nord, pos(R,C), pos(R,C),_).

occupata(pos(R,C), ListaEl) :-
    member([_,pos(R, C)], ListaEl);
    muro(pos(R,C));
    ghiaccio(pos(R,C)).

modifica_posizione([], _,_, []).

modifica_posizione([[Tipo,pos(R,C)] | Resto], CurrentPos,NewPos, [[Tipo,ElPos]| NuovoResto]) :-
    (   pos(R,C) = CurrentPos -> ElPos = NewPos; ElPos = pos(R,C) ),
    modifica_posizione(Resto, CurrentPos, NewPos, NuovoResto).

ha_martello([pos(R,C)| _], true) :- martello(pos(R, C)).
ha_martello(_, _).
