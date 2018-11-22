:- use_module(library(lists)).

% to set the current games pocket: asserta(pocket(card_,_), card(_,_)).
% to set the table: retract + assert(table([card(_,_),...]))
% @< used as arbitrary ordering to remove duplicate answers


% test card set
test([card(3,d), card(4,h)]).



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

pairs(CARDS, R) :- findall(pair(A,B, CARDS), (member(A, CARDS), member(B, CARDS), pair(A,B)), R).

twopairs(CARDS, R) :- findall(twopair(A,B,C,D), (member(A, CARDS), member(B, CARDS), member(C, CARDS), member(D, CARDS), twopair(A,B,C,D)), R).

threes(CARDS, R) :- findall(three(A,B,C), (member(A, CARDS), member(B, CARDS), member(C, CARDS), three(A,B,C)), R).

straights(CARDS, R) :- findall(straight(A,B,C,D,E), (member(A, CARDS), member(B, CARDS), member(C, CARDS), member(D, CARDS), member(E, CARDS), straight(A,B,C,D,E)), R).

flushes(CARDS, R) :- findall(flush(A,B,C,D,E), (member(A, CARDS), member(B, CARDS), member(C, CARDS), member(D, CARDS), member(E, CARDS), flush(A,B,C,D,E)), R).

hands(CARDS, R) :- highs(CARDS, HIGHS), pairs(CARDS, PAIRS), twopairs(CARDS, TWOPAIRS), threes(CARDS, THREES), straights(CARDS, STRAIGHTS), flushes(CARDS, FLUSHES), append(HIGHS, PAIRS, ONE), append(ONE, TWOPAIRS, TWO), append(TWO, THREES, THREE), append(THREE, STRAIGHTS, FOUR), append(FOUR, FLUSHES, R).

% find best hand for individual player.
best([C],C).
best([H|T],TM) :- ranking(H,HR), ranking(TM, TMR), TMR > HR, best(T, TM).
best([H|T],H) :- ranking(H,HR), ranking(TM, TMR), TMR < HR, best(T, TM).
best([H|T],TM) :- ranking(H,HR), ranking(TM, TMR), HR == TMR, best(T, TM), beats(TM, H).
best([H|T],TM) :- ranking(H,HR), ranking(TM, TMR), HR == TMR, best(T, TM), beats(H, TM).

beats(high(A), high(B)) :- value(A,VA), value(B,VB), VA >= VB.
beats(pair(A,_), pair(B,_)) :- value(A,VA), value(B,VB), VA >= VB.

tiebreak(pair(A1,B1), pair(A2,B2), CARDSP1, CARDP2) :- subtract(CARDSP1, [A1,B1], P1), subtract(CARDS, [A2,B2], P2), cardsum(P1, X1), cardsum(P2, X2), X1 >= X2.

% sum of card values for tiebreaks
cardsum([C], X) :- value(C,X).
cardsum([H|T], X) :- value(H,V), cardsum(T, Q), X is V + Q.
