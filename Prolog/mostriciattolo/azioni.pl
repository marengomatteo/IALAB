
/*
    in base all'azione compiuta (nord,sud,est,ovest) il mostriciattolo e le gemme dovranno muoversi fino alla fine della board
    
    check da fare:
        contollare che sia le gemme sia il mostriciattolo non escano dalla board
    
    facciamo un predicato generico che muova tutto?

    caso 1 da gestire:
        se il mostriciattolo si trova in pos(1,5) e la gemma in pos(1,6)
        quando l'azione è est il mostriciattolo non dovrà andare sopra la gemma quindi
        le posizioni finali dovranno essere pos(1,7) e pos(1,8) idem per il viceversa
        come controlliamo questa cosa?

    caso 2 da gestire:
        se la gemma 1 si trova in pos(1,5) e la gemma 2 in pos(1,6)
        quando l'azione è est la gemma 1 non dovrà andare sopra la gemma 2 quindi
        le posizioni finali dovranno essere pos(1,7) e pos(1,8) idem per il viceversa
        come controlliamo questa cosa?
*/

occupata(pos(R,C)) :-
    gemma(pos(R,C)).

controllo_oggetti(Az,pos(R,C)) :-
    gemma(pos(R,C)),
    priorita(Az, pos(R,C)),
    muovi_gemma(Az).

controllo_oggetti(Az,pos(R,C)) :-
    mostriciattolo(pos(R,C)),
    priorita(Az, pos(R,C)),
    muovi_mostriciattolo(Az).

controllo_oggetti(Az,pos(R,C)).


/* priorità est */
priorita(est,pos(R,C)) :-
    C1 is C+1,
    C1 < 9,
    controllo_oggetti(pos(R,C1)),
    priorita(est, pos(R,C1)).

priorita(est,pos(R,C)).

/* priorità sud */
priorita(sud,pos(R,C)) :-
    R1 is R+1,
    R1 < 9,
    controllo_oggetti(pos(R1,C)),
    priorita(sud, pos(R1,C)).

priorita(sud,pos(R,C)).

/* priorità ovest */
priorita(ovest,pos(R,C)) :-
    C1 is C-1,
    C1 > 1,
    controllo_oggetti(pos(R,C1)),
    priorita(ovest, pos(R,C1)).

priorita(ovest,pos(R,C)).

/* priorità nord */
priorita(nord,pos(R,C)) :-
    R1 is R-1,
    R1 > 1 ,
    controllo_oggetti(pos(R1,C)),
    priorita(nord, pos(R1,C)).

priorita(nord,pos(R,C)).

/* posizione valida est */
posizione_valida(est,pos(R,C),pos(R,C1))
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

/* posizione valida ovest */
posizione_valida(ovest,pos(R,C),pos(R,C1))
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
rovesciamento(Az) :-


/* muovi mostriciattolo */



/* muovi gemme */
