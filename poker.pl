:- use_module(library(lists)).

% to set the current games pocket: asserta(pocket(card_,_), card(_,_)).
% to set the table: retract + assert(table([card(_,_),...]))
% @< used as arbitrary ordering to remove duplicate answers

% test card set
testP1([card(ace,d), card(ace,h), card(5,h)]).
testP2([card(ace,c), card(ace,s), card(4,h)]).


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

% get max rank from a set of cards
maxrank([C], R) :- value(C, R).
maxrank([H|T], RR) :- maxrank(T, RR), value(H, HR), RR >= HR.
maxrank([H|T], HR) :- maxrank(T, RR), value(H, HR), HR >= RR.

% for straights
order(card(ace, _), 1).
order(card(R,S), V):- dif(R, ace), value(card(R,S), V).

% hands
royalflush(card(ace, S), card(king, S), card(queen, S), card(jack, S), card(10, S)).
straightflush(card(A,S),card(B,S),card(C,S),card(D,S),card(E,S)) :- order(A, OA),order(B, OB),order(C, OC),order(D, OD),order(E, OE), OA is OB-1, OB is OC-1, OC is OD-1, OD is OE-1.
four(card(A,SW), card(A,SX), card(A,SY), card(A,SZ)) :- SW @< SX, SX @< SY, SY @< SZ.
fullhouse(card(A,AS1), card(A,AS2), card(A,AS3), card(B,BS1), card(B,BS2)) :- AS1 @< AS2, AS2 @< AS3, BS1 @< BS2.
flush(card(A,S), card(B,S), card(C,S), card(D,S), card(E,S)) :- A @< B, B @< C, C @< D, D @< E.
straight(A,B,C,D,E) :- order(A, OA),order(B, OB),order(C, OC),order(D, OD),order(E, OE), OA is OB-1, OB is OC-1, OC is OD-1, OD is OE-1, \+ straightflush(A,B,C,D,E).
three(card(A,SX), card(A, SY), card(A, SZ)) :- SX @< SY, SY @< SZ.
twopair(A,B,C,D) :- pair(A,B), pair(C,D), A @< C.
pair(card(A,SX), card(A,SY)) :- SX @< SY.
high(card(_,_)).

% hand ranks
ranking(high(_), 0).
ranking(pair(_,_), 1).
ranking(twopair(_,_,_,_), 2).
ranking(three(_,_,_), 3).
ranking(straight(_,_,_,_,_), 4).
ranking(flush(_,_,_,_,_), 5).
ranking(fullhouse(_,_,_,_,_), 6).
ranking(four(_,_,_,_), 7).
ranking(straightflush(_,_,_,_,_), 8).
ranking(royalflush(_,_,_,_,_), 9).


highs(CARDS, R) :- findall(high(A), (member(A, CARDS), high(A)), R).
pairs(CARDS, R) :- findall(pair(A,B), (member(A, CARDS), member(B, CARDS), pair(A,B)), R).
twopairs(CARDS, R) :- findall(twopair(A,B,C,D), (member(A, CARDS), member(B, CARDS), member(C, CARDS), member(D, CARDS), twopair(A,B,C,D)), R).
threes(CARDS, R) :- findall(three(A,B,C), (member(A, CARDS), member(B, CARDS), member(C, CARDS), three(A,B,C)), R).
straights(CARDS, R) :- findall(straight(A,B,C,D,E), (member(A, CARDS), member(B, CARDS), member(C, CARDS), member(D, CARDS), member(E, CARDS), straight(A,B,C,D,E)), R).
flushes(CARDS, R) :- findall(flush(A,B,C,D,E), (member(A, CARDS), member(B, CARDS), member(C, CARDS), member(D, CARDS), member(E, CARDS), flush(A,B,C,D,E)), R).
fullhouses(CARDS, R) :- findall(fullhouse(A,B,C,D,E), (member(A, CARDS), member(B, CARDS), member(C, CARDS), member(D, CARDS), member(E, CARDS), fullhouse(A,B,C,D,E)), R).
fours(CARDS, R) :- findall(four(A,B,C,D), (member(A, CARDS), member(B, CARDS), member(C, CARDS), member(D, CARDS), four(A,B,C,D)), R).
straightflushes(CARDS, R) :- findall(straightflush(A,B,C,D,E), (member(A, CARDS), member(B, CARDS), member(C, CARDS), member(D, CARDS), member(E, CARDS), straightflush(A,B,C,D,E)), R).
royalflushes(CARDS, R) :- findall(royalflush(A,B,C,D,E), (member(A, CARDS), member(B, CARDS), member(C, CARDS), member(D, CARDS), member(E, CARDS), royalflush(A,B,C,D,E)), R).


% generate all hands
hands(CARDS, R) :- highs(CARDS, HIGHS), pairs(CARDS, PAIRS), twopairs(CARDS, TWOPAIRS), threes(CARDS, THREES), straights(CARDS, STRAIGHTS), flushes(CARDS, FLUSHES), fullhouses(CARDS, FULLHOUSES), fours(CARDS, FOURS), straightflushes(CARDS, STRAIGHTFLUSHES), royalflushes(CARDS, ROYALFLUSHES), append(HIGHS, PAIRS, ONE), append(ONE, TWOPAIRS, TWO), append(TWO, THREES, THREE), append(THREE, STRAIGHTS, FOUR), append(FOUR, FLUSHES, FIVE), append(FIVE, FULLSHOUSES, SIX), append(SIX, FOURS, SEVEN), append(SEVEN, STRAIGHTFLUSHES, EIGHT), append(EIGHT, ROYALFLUSHES, R).

% find best hand for individual player.
best([C],C).
best([H|T],TM) :- ranking(H,HR), ranking(TM, TMR), TMR > HR, best(T, TM).
best([H|T],H) :- ranking(H,HR), ranking(TM, TMR), TMR < HR, best(T, TM).
best([H|T],TM) :- ranking(H,HR), ranking(TM, TMR), HR == TMR, best(T, TM), beats(TM, H).
best([H|T],H) :- ranking(H,HR), ranking(TM, TMR), HR == TMR, best(T, TM), beats(H, TM).
% choose random card if neither beats the other
best([H|T],TM) :- ranking(H,HR), ranking(TM, TMR), HR == TMR, best(T, TM), \+beats(H, TM), \+beats(TM, H).

% same as above but false if no hand is superior (nicer way to do this?).
bestS([C],C).
bestS([H|T],TM) :- ranking(H,HR), ranking(TM, TMR), TMR > HR, bestS(T, TM).
bestS([H|T],H) :- ranking(H,HR), ranking(TM, TMR), TMR < HR, bestS(T, TM).
bestS([H|T],TM) :- ranking(H,HR), ranking(TM, TMR), HR == TMR, bestS(T, TM), beats(TM, H).
bestS([H|T],H) :- ranking(H,HR), ranking(TM, TMR), HR == TMR, bestS(T, TM), beats(H, TM).

% determine superiority between similar hands
beats(high(A), high(B)) :- value(A,VA), value(B,VB), VA > VB.
beats(pair(A,_), pair(B,_)) :- value(A,VA), value(B,VB), VA > VB.
beats(twopair(A1,_,_,_), twopair(A2,_,B2,_)) :- value(A1, VA1), value(A2, VA2), value(B2,VB2), VA1 > VA2, VA1 > VB2.
beats(twopair(_,_,B1,_), twopair(A2,_,B2,_)) :- value(A2, VA2), value(B1, VB1), value(B2,VB2), VB1 > VA2, VB1 > VB2.
beats(three(A,_,_), three(B,_,_)) :- value(A,VA), value(B,VB), VA > VB.
beats(straight(A,_,_,_,_), straight(B,_,_,_,_)) :- value(A,VA), value(B,VB), VA > VB.


% tiebreak between same hands
tiebreak(pair(A1,B1), pair(A2,B2), CARDSP1, CARDSP2) :- subtract(CARDSP1, [A1,B1], RP1), subtract(CARDSP2, [A2,B2], RP2), maxrank(RP1, MRP1), maxrank(RP2, MRP2), MRP1 > MRP2.
tiebreak(twopair(A1,B1,C1,D1), twopair(A2,B2,C2,D2), CARDSP1, CARDSP2) :- subtract(CARDSP1, [A1,B1,C1,D1], RP1), subtract(CARDSP2, [A2,B2,C2,D2], RP2), maxrank(RP1, MRP1), maxrank(RP2, MRP2), MRP1 > MRP2.

% W is 1 if P1 wins, and 2 if P2 wins
winner(CARDSP1, CARDSP2, 1) :- hands(CARDSP1, HANDSP1), hands(CARDSP2, HANDSP2), append(HANDSP1, HANDSP2, ALLHANDS), bestS(ALLHANDS, X), member(X, HANDSP1).
winner(CARDSP1, CARDSP2, 2) :- hands(CARDSP1, HANDSP1), hands(CARDSP2, HANDSP2), append(HANDSP1, HANDSP2, ALLHANDS), bestS(ALLHANDS, X), member(X, HANDSP2).
% neither have a winning hand, go to tiebreak
winner(CARDSP1, CARDSP2, 1) :- hands(CARDSP1, HANDSP1), hands(CARDSP2, HANDSP2), best(HANDSP1, BESTP1), best(HANDSP2, BESTP2), tiebreak(BESTP1, BESTP2, CARDSP1, CARDSP2).
winner(CARDSP1, CARDSP2, 2) :- hands(CARDSP1, HANDSP1), hands(CARDSP2, HANDSP2), best(HANDSP1, BESTP1), best(HANDSP2, BESTP2), tiebreak(BESTP2, BESTP1, CARDSP2, CARDSP1).
% there could be no winner (split pot).
