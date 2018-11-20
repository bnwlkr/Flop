% to set the current games pocket: asserta(pocket(card_,_), card(_,_)).
% to set the table: retract + assert(table([card(_,_),...]))
% @< used as arbitrary ordering to remove duplicate answers

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

% card values
value(card(jack, _), 11).
value(card(queen, _), 12).
value(card(king, _), 13).
value(card(ace, _), 14).
value(card(N, _), N) :- number(N), rank(N).

% for straights
order(card(ace, _), 1).
order(card(R,S), V):- dif(R, ace), value(card(R,S), V).

deck(A) :- findall((Suit, Rank), card(Suit, Rank), A).

% hands
flush(card(A,R), card(B,R), card(C,R), card(D,R), card(E,R)) :- A @< B, B @< C, C @< D, D @< E.

straight(A,B,C,D,E) :- order(A, OA),order(B, OB),order(C, OC),order(D, OD),order(E, OE), OA is OB - 1, OB is OC - 1, OC is OD - 1, OD is OE - 1.

three(card(A,SX), card(A, SY), card(A, SZ)) :- SX @< SY, SY @< SZ.

twopair(A,B,C,D) :- pair(A,B), pair(C,D), A @< C, dif(A,B), dif(A,C), dif(A,D), dif(B,C), dif(B,D).

pair(card(A,SX), card(A,SY)) :- SX @< SY.

high(card(_,_)).


ranking(high(_), 0).
ranking(pair(_,_), 1).
ranking(twopair(_,_,_,_), 2).
ranking(three(_,_,_), 3).
ranking(straight(_,_,_,_,_), 4).
ranking(flush(_,_,_,_,_), 5).


highs(CARDS, R) :- findall(high(A), (member(A, CARDS), high(A)), R).

pairs(CARDS, R) :- findall(pair(A,B), (member(A, CARDS), member(B, CARDS), pair(A,B)), R).

twopairs(CARDS, R) :- findall(twopair(A,B,C,D), (member(A, CARDS), member(B, CARDS), member(C, CARDS), member(D, CARDS), twopair(A,B,C,D)), R).

threes(CARDS, R) :- findall(three(A,B,C), (member(A, CARDS), member(B, CARDS), member(C, CARDS), three(A,B,C)), R).

straights(CARDS, R) :- findall(straight(A,B,C,D,E), (member(A, CARDS), member(B, CARDS), member(C, CARDS), member(D, CARDS), member(E, CARDS), straight(A,B,C,D,E)), R).

flushes(CARDS, R) :- findall(flush(A,B,C,D,E), (member(A, CARDS), member(B, CARDS), member(C, CARDS), member(D, CARDS), member(E, CARDS), flush(A,B,C,D,E)), R).


hands(CARDS, R) :- highs(CARDS, HIGHS), pairs(CARDS, PAIRS), twopairs(CARDS, TWOPAIRS), threes(CARDS, THREES), straights(CARDS, STRAIGHTS), flushes(CARDS, FLUSHES), append(HIGHS, PAIRS, ONE), append(ONE, TWOPAIRS, TWO), append(TWO, THREES, THREE), append(THREE, STRAIGHTS, FOUR), append(FOUR, FLUSHES, R).

% THIS STUFF DOESNT WORK BECAUSE OF THE WAY THAT TIES ARE BROKEN...

%best([C], C).
%best([H|T], SF) :- ranking(H, RANK), ranking(SF, SFRANK), RANK == SFRANK, beats(H,SF), best(T, H).
%best([H|T], SF) :- ranking(H, RANK), ranking(SF, SFRANK), RANK == SFRANK, beats(SF,H), best(T, SF).
%best([H|T], SF) :- ranking(H, RANK), ranking(SF, SFRANK), RANK > SFRANK, best(T, H).
%best([H|T], SF) :- ranking(H, RANK), ranking(SF, SFRANK), RANK < SFRANK, best(T, SF).

%beats(high(A), high(B)) :- value(A, VA), value(B, VB), VA > VB.
%beats(pair(A,_), pair(B,_)) :- value(A, VA), value(B, VB), VA > VB.
%beats(twopair(A,_,C,_), twopair(B,_,D,_)) :- value(A,VA), value(B,VB), value(C,VC), value(D,VD), VA + VC < VB + VD.
%beats(three(A,_,_), three(B,_,_)) :- value(A, VA), value(B, VB), VA > VB.
%beats(straight(A,_,_,_,_), straight(B,_,_,_,_)) :- value(A, VA), value(B, VB), VA > VB.
%beats(straight(A,B,C,D,_), straight(B,_,_,_,_))

