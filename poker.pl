% to set the current games pocket: asserta(pocket(card_,_), card(_,_)).
% to set the table: retract + assert(table([card(_,_),...]))

rank(    2).
rank(    3).
rank(    4).
rank(    5).
rank(    6).
rank(    7).
rank(    8).
rank(    9).
rank(   10).
rank( jack).
rank(queen).
rank( king).
rank(  ace).

suit(d).
suit(h).
suit(s).
suit(c).

order(suit(d), 0).
order(suit(h), 1).
order(suit(s), 2).
order(suit(c), 3).

card(Rank, Suit) :- rank(Rank), suit(Suit).

value(card(jack, _), 11).
value(card(queen, _), 12).
value(card(king, _), 13).
value(card(ace, _), 14).
value(card(N, _), N) :- number(N), rank(N).

deck(A) :- findall((Suit, Rank), card(Suit, Rank), A).

% hands

three(card(A,SX), card(A, SY), card(A, SZ)) :- dif(SX,SY), dif(SY, SZ), dif(SX,SZ).
twopair(A,B,C,D) :- pair(A,B), pair(C,D).
pair(card(A,SX), card(A,SY)) :- dif(SX,SY).
high(card(_,_)).




% finds pairs only for now (TODO: remove duplicates)
hands(CARDS, R) :- findall(pair(A,B), (member(A, CARDS), member(B, CARDS), pair(A,B)), R).



