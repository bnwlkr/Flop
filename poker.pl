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

% for card values
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
hand(flush, card(A,R), card(B,R), card(C,R), card(D,R), card(E,R)) :- A @< B, B @< C, C @< D, D @< E.

hand(straight, A,B,C,D,E) :- order(A, OA),order(B, OB),order(C, OC),order(D, OD),order(E, OE), OA is OB - 1, OB is OC - 1, OC is OD - 1, OD is OE - 1.

hand(three, card(A,SX), card(A, SY), card(A, SZ)) :- SX @< SY, SY @< SZ.

hand(twopair, A,B,C,D) :- hand(pair,A,B), hand(pair,C,D), A @< C, dif(A,B), dif(A,C), dif(A,D), dif(B,C), dif(B,D).

hand(pair, card(A,SX), card(A,SY)) :- SX @< SY.
% TODO: add rest



pairs(CARDS, R) :- findall(hand(pair,A,B), (member(A, CARDS), member(B, CARDS), hand(pair,A,B)), R).

twopairs(CARDS, R) :- findall(hand(twopair,A,B,C,D), (member(A, CARDS), member(B, CARDS), member(C, CARDS), member(D, CARDS), hand(twopair,A,B,C,D)), R).

threes(CARDS, R) :- findall(hand(three,A,B,C), (member(A, CARDS), member(B, CARDS), member(C, CARDS), hand(three,A,B,C)), R).

straights(CARDS, R) :- findall(hand(straight,A,B,C,D,E), (member(A, CARDS), member(B, CARDS), member(C, CARDS), member(D, CARDS), member(E, CARDS), hand(straight,A,B,C,D,E)), R).

flushes(CARDS, R) :- findall(hand(flush,A,B,C,D,E), (member(A, CARDS), member(B, CARDS), member(C, CARDS), member(D, CARDS), member(E, CARDS), hand(flushA,B,C,D,E)), R).
% TODO: add rest

% all possible hands from CARDS
hands(CARDS, R) :- pairs(CARDS, PAIRS), twopairs(CARDS, TWOPAIRS), threes(CARDS, THREES), straights(CARDS, STRAIGHTS), flushes(CARDS, FLUSHES), append(PAIRS, TWOPAIRS, ONE), append(ONE, THREES, TWO), append(TWO, FLUSHES, THREE), append(THREE, STRAIGHTS, R).
% TODO: add rest







