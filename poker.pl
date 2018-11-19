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

card(Rank, Suit) :- rank(Rank), suit(Suit).

value(card(jack, _), 11).
value(card(queen, _), 12).
value(card(king, _), 13).
value(card(ace, _), 14).
value(card(N, _), N) :- number(N), rank(N).

deck(A) :- findall((Suit, Rank), card(Suit, Rank), A).

diff_order(SX, SY) :- dif(SX, SY), SX @< SY.

% hands
three(card(A,SX), card(A, SY), card(A, SZ)) :- diff_order(SX,SY), diff_order(SY, SZ).
twopair(A,B,C,D) :- pair(A,B), pair(C,D), diff_order(A,C), dif(A,B), dif(A,C), dif(A,D), dif(B,C), dif(B,D).
pair(card(A,SX), card(A,SY)) :- diff_order(SX, SY).


% return a list of pairs
hands(CARDS, R) :- findall(pair(A,B), (member(A, CARDS), member(B, CARDS), pair(A,B)), R).
% return a list of twopairs
hands(CARDS, R) :- findall(twopair(A,B,C,D), (member(A, CARDS), member(B, CARDS), member(C, CARDS), member(D, CARDS), twopair(A,B,C,D)), R).
% return a list of threes
hands(CARDS, R) :- findall(three(A,B,C), (member(A, CARDS), member(B, CARDS), member(C, CARDS), three(A,B,C)), R).



