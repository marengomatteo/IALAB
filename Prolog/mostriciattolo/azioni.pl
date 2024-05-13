/*
    azione applicabile:
        - controlliamo che non sia occupata da un muro fisso
        - controlliamo che non sia presente del ghiaccio
*/
/* per applicabile controllo anche che il ghiaccio sia abbattibile o no, quindi se ho il martello è applicabile*/

occupata(pos(R,C)) :- 
    muro(pos(R,C));
    gemma(pos(R,C)).
    /*(\+possiede(martello), ghiaccio(pos(R,C))).*/


applicabile(nord,pos(R,C)) :- 
    R>1,
    R1 is R-1,
    \+ occupata(pos(R1,C)).

applicabile(ovest,pos(R,C)) :- 
    C>1,
    C1 is C-1,
    \+ occupata(pos(R,C1)).

applicabile(sud,pos(R,C)) :- 
    num_righe(NR), R<NR,
    R1 is R+1,
    \+ occupata(pos(R1,C)).

applicabile(est,pos(R,C)) :- 
    num_col(NC), C<NC,
    C1 is C+1,
    \+ occupata(pos(R,C1)).


nonRipetere(_,[]).
nonRipetere(Az, [LastAz|_]) :- Az \= LastAz.


/* --- TRASFORMA EST --- */
trasforma(est,pos(R,C),pos(R,C1)) :- 
    applicabile(est,pos(R,C)),
    C2 is C+1,
    raccogli(pos(R,C2)),
    trasforma(est,pos(R,C2), pos(R,C1)).

trasforma(est,pos(R,C),pos(R,C1)) :- 
    applicabile(est,pos(R,C)),
    C2 is C+1,
    trasforma(est,pos(R,C2), pos(R,C1)).

trasforma(est,pos(R,C),pos(R,C)).

/* --- TRASFORMA OVEST --- */

trasforma(ovest,pos(R,C), _ , pos(R,C1)) :-
    applicabile(ovest,pos(R,C)),
    C2 is C-1,
    raccogli(pos(R,C2)),
    trasforma(ovest,pos(R,C2),ovest, pos(R,C1)).

trasforma(ovest,pos(R,C), _ ,pos(R,C1) ) :- 
    applicabile(ovest, pos(R,C)),
    C2 is C-1,
    trasforma(ovest,pos(R,C2), ovest , pos(R,C1)).

trasforma(ovest,pos(R,C), LastAz , pos(R,C)) :- LastAz =:= ovest. 

/* --- TRASFORMA SUD --- */

trasforma(sud,pos(R,C),pos(R1,C)) :- 
    applicabile(sud, pos(R,C)),
    R2 is R+1,
    raccogli(pos(R2,C)),
    trasforma(sud,pos(R2,C), pos(R1,C)).

trasforma(sud,pos(R,C),pos(R1,C)) :- 
    applicabile(sud, pos(R,C)),
    R2 is R+1,
    trasforma(sud,pos(R2,C), pos(R1,C)).

trasforma(sud,pos(R,C),pos(R,C)). 

/* --- TRASFORMA NORD --- */

trasforma(nord,pos(R,C),pos(R1,C)) :- 
    applicabile(nord,pos(R,C)),
    R2 is R-1,
    raccogli(pos(R2,C)),
    trasforma(nord,pos(R2,C), pos(R1,C)).

trasforma(nord,pos(R,C),pos(R1,C)) :- 
    applicabile(nord,pos(R,C)),
    R2 is R-1,
    trasforma(nord,pos(R2,C), pos(R1,C)).

trasforma(nord,pos(R,C),pos(R,C)).

% raccogli(pos(R,C)) :-
%     martello(pos(R,C)),
%     \+ possiede(martello),
%     assert(possiede(martello)).

raccogli(_) :- 1>1.


muovi_gemme(Dir) :-
    findall(pos(R,C), gemma(pos(R,C)), Gemme),  /*Trova tutte le pos delle gemme */
    muovi_lista_gemme(Dir,Gemme).

muovi_lista_gemme(_,[]). /*Se non ci sono più gemme da muovere, termina*/
muovi_lista_gemme(Dir, [pos(R,C)|Tail]) :-
    trasforma(Dir,pos(R,C), pos(R1,C)),
    retract(gemma(pos(R,C))), /*Possiamo usare il retract?? Altrimenti dobbiamo tenere traccia della lista di posizioni delle gemme e passarla come parametro a ogni chiamata*/
    assert(gemma(pos(R1,C))),
    muovi_lista_gemme(Dir, Tail).

adiacenti(pos(R1,C1), pos(R2,C2)) :-
    (R1 =:= R2, abs(C1 - C2) =:= 1);
    (C1 =:= C2, abs(R1 - R2) =:= 1).

contigue_due_a_due(pos(R1,C1), pos(R2,C2), pos(R3,C3)) :-
    (adiacenti(pos(R1,C1), pos(R2,C2)), adiacenti(pos(R2,C2), pos(R3,C3)));
    (adiacenti(pos(R1,C1), pos(R3,C3)), adiacenti(pos(R3,C3), pos(R2,C2))).