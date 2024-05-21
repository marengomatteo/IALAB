
aggiungi_gemma(Gemma) :-
    assertz(gemma(Gemma)).

rimuovi_gemma(Gemma) :-
    retract(gemma(Gemma)).

aggiungi_mostriciattolo(Pos) :-
    assertz(mostriciattolo(Pos)).

rimuovi_mostriciattolo(Pos) :-
    retract(mostriciattolo(Pos)).

occupata(pos(R,C)) :-
    gemma(pos(R,C));
    mostriciattolo(pos(R,C));
    muro(pos(R,C)).

controllo_oggetti(Az,pos(R,C)) :-
    gemma(pos(R,C)),
    priorita(Az, pos(R,C)),
    rimuovi_gemma(pos(R,C)),
    posizione_valida(Az, pos(R,C), pos(R1,C1)),
	aggiungi_gemma(pos(R1,C1)).

controllo_oggetti(Az,pos(R,C)) :-
    mostriciattolo(pos(R,C)),
    priorita(Az, pos(R,C)),
    rimuovi_mostriciattolo(pos(R,C)),
    posizione_valida(Az, pos(R,C), pos(R1,C1)),
    aggiungi_mostriciattolo(pos(R1,C1)).

controllo_oggetti(_,_).

/* priorità est */
priorita(est,pos(R,C)) :-
    C1 is C+1,
    C1 < 9,
    controllo_oggetti(est,pos(R,C1)),
    priorita(est, pos(R,C1)).

priorita(est,_).

/* priorità sud */
priorita(sud,pos(R,C)) :-
    R1 is R+1,
    R1 < 9,
    controllo_oggetti(sud,pos(R1,C)),
    priorita(sud, pos(R1,C)).

priorita(sud,_).

/* priorità ovest */
priorita(ovest,pos(R,C)) :-
    C1 is C-1,
    C1 > 1,
    controllo_oggetti(ovest,pos(R,C1)),
    priorita(ovest, pos(R,C1)).

priorita(ovest,_).

/* priorità nord */
priorita(nord,pos(R,C)) :-
    R1 is R-1,
    R1 > 1 ,
    controllo_oggetti(nord,pos(R1,C)),
    priorita(nord, pos(R1,C)).

priorita(nord,_).

/* TODO: da rinominare*/
/* --- posizione valida EST --- */
posizione_valida(est,pos(R,C),pos(R,C1)) :-
    C < 8,
    C2 is C+1,
    \+ occupata(pos(R,C2)),
    posizione_valida(est, pos(R,C2),pos(R,C1)).

posizione_valida(est, pos(R,C), pos(R, C)).

/* --- posizione valida SUD --- */

posizione_valida(sud, pos(R,C), pos(R1,C)) :-
    R < 8,
    R2 is R+1,
    \+ occupata(pos(R2,C)),
    posizione_valida(sud, pos(R2,C), pos(R1,C)).

posizione_valida(sud, pos(R,C), pos(R,C)).

/* --- posizione valida OVEST --- */
posizione_valida(ovest,pos(R,C),pos(R,C1)) :-
    C > 1,
    C2 is C-1,
    \+ occupata(pos(R,C2)),
    posizione_valida(ovest, pos(R,C2),pos(R,C1)).

posizione_valida(ovest, pos(R,C), pos(R, C)).

/* --- posizione valida NORD --- */

posizione_valida(nord, pos(R,C), pos(R1,C)) :-
    R > 1,
    R2 is R-1,
    \+ occupata(pos(R2,C)),
    posizione_valida(nord, pos(R2,C), pos(R1,C)).

posizione_valida(nord, pos(R,C), pos(R,C)).


/* rovescia la board */
    
rovesciamento(est) :-
    righe_elementi(Lista),
    itera_righe(est,Lista).

rovesciamento(nord) :-
    colonne_elementi(Lista),
	itera_colonne(nord,Lista).

rovesciamento(ovest) :-
    righe_elementi(Lista),
    itera_righe(ovest,Lista).

rovesciamento(sud) :-
    colonne_elementi(Lista),
	itera_colonne(sud,Lista).

righe_elementi(ListaRighe) :-
    findall(R, 
            (gemma(pos(R,_)); mostriciattolo(pos(R,_))), 
            Righe),
    sort(Righe, ListaRighe).

colonne_elementi(ListaColonne) :-
    findall(C, 
            (gemma(pos(_,C)); mostriciattolo(pos(_,C))), 
            Colonne),
    sort(Colonne, ListaColonne).

itera_righe(_,[]).
itera_righe(Az,[R|Tail]) :-
    primo_elemento(Az,pos(R,_)),
    itera_righe(Az,Tail).

itera_colonne(_,[]).
itera_colonne(Az,[C|Tail]) :-
    primo_elemento(Az,pos(_,C)),
    itera_colonne(Az,Tail).


/* --- PRIMO ELEMENTO EST --- */
primo_elemento(est,pos(R,C)) :-
  (
  	gemma(pos(R, C));
  	mostriciattolo(pos(R, C))
  ),
  \+ (
    	(gemma(pos(R,C1)); mostriciattolo(pos(R, C1)) ), 
    	C1 < C
    ),
    controllo_oggetti(est,pos(R,C)).

/* --- PRIMO ELEMENTO OVEST --- */
primo_elemento(ovest,pos(R,C)) :-
  (
  	gemma(pos(R, C));
  	mostriciattolo(pos(R, C))
  ),
  \+ (
    	(gemma(pos(R,C1)); mostriciattolo(pos(R, C1)) ), 
    	C1 > C
    ),
    controllo_oggetti(ovest,pos(R,C)).

/* --- PRIMO ELEMENTO NORD --- */
primo_elemento(nord,pos(R,C)) :-
  (
  	gemma(pos(R, C));
  	mostriciattolo(pos(R, C))
  ),
  \+ (
    	(gemma(pos(R1,C)); mostriciattolo(pos(R1, C)) ), 
    	R < R1
    ),
    controllo_oggetti(nord,pos(R,C)).

/* --- PRIMO ELEMENTO SUD --- */
primo_elemento(sud,pos(R,C)) :-
  (
  	gemma(pos(R, C));
  	mostriciattolo(pos(R, C))
  ),
  \+ (
    	(gemma(pos(R1,C)); mostriciattolo(pos(R1, C)) ), 
    	R > R1
    ),
    controllo_oggetti(sud,pos(R,C)).


nonRipetere(est, [LastAz|_]) :- est \= LastAz.
nonRipetere(sud, [LastAz|_]) :- sud \= LastAz.
nonRipetere(ovest, [LastAz|_]) :- ovest \= LastAz.
nonRipetere(nord, [LastAz|_]) :- nord \= LastAz.
