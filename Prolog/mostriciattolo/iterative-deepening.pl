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