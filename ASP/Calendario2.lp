% Definizione delle squadre atalanta, bologna, empoli, fiorentina, inter, juventus, lazio, lecce, milan, monza, napoli, roma, salernitana, sassuolo, torino, udinese).
squadra(atalanta).
squadra(bologna).
squadra(empoli).
squadra(fiorentina).
squadra(inter).
squadra(juventus).
squadra(lazio).
squadra(lecce).
squadra(milan).
squadra(monza).
squadra(napoli).
squadra(roma).
squadra(salernitana).
squadra(sassuolo).
squadra(torino).
squadra(udinese).

% Definizione dei derby
derby(inter, milan).
derby(juventus, torino).
derby(lazio, roma).

% Generazione dei match
{ match(X, Y, Giornata) : squadra(X), squadra(Y), X != Y } = 8 :- Giornata = 1..30.

%tolgo la possibilità che una squadra giochi più match in una giornata considerando anche che sia fuori o in casa
:- match(X, Y, Giornata), match(X, Z, Giornata), Y != Z.
:- match(X, Y, Giornata), match(Z, X, Giornata).
:- match(Y, X, Giornata), match(Z, X, Giornata), Y!=Z.
%tolgo la possibilità che una squadra giochi più match con la stessa squadra in giornate diverse
:- match(X, Y, Giornata1), match(X, Y, Giornata2), Giornata1 != Giornata2.
:- match(X, Y, Giornata1), match(Y, X, Giornata2), Giornata1 < 16, Giornata2 < 16.
:- match(X, Y, Giornata1), match(Y, X, Giornata2), Giornata1 > 15, Giornata2 > 15.
:- derby(X, Y), match(X, Z, Giornata), match(Y, Q, Giornata).


#show match/3.