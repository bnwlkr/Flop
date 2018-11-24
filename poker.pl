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
value(card(N, _), N) :- rank(N), number(N).

% suit values
suitvalue(card(_, d), 3).
suitvalue(card(_, h), 2).
suitvalue(card(_, s), 1).
suitvalue(card(_, c), 0).

% get max rank from a set of cards
maxrank([C], C).
maxrank([H|T], R) :- maxrank(T, R), value(H, HR), value(R, RR), RR > HR.
maxrank([H|T], H) :- maxrank(T, R), value(H, HR), value(R, RR), HR > RR.
maxrank([H|T], R) :- maxrank(T, R), value(H, HR), value(R, RR), HR = RR, suitvalue(H, HS), suitvalue(R, RS), RS > HS.
maxrank([H|T], H) :- maxrank(T, R), value(H, HR), value(R, RR), HR = RR, suitvalue(H, HS), suitvalue(R, RS), HS > RS.

% remove get card with the given rank
removecardwithrank([], _, 0, []).
removecardwithrank([card(V, _)|T], V, N, R) :- removecardwithrank(T, V, NSUB, R), N is NSUB+1.
removecardwithrank([H|T], V, N, [H|R]) :- value(H, HV), dif(HV, V), removecardwithrank(T, V, R).

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
ranking(high(A), [R]) :- value(A, R).
ranking(pair(A,B), [R]) :- value(A, V), R is 100+V.
ranking(twopair(A,B,C,D), [R1, R2]) :- value(C, V1), R1 is 200+V1, value(A, V2), R2 is 200+V2.
ranking(three(A,B,C), [R]) :- value(A, V), R is 300+V.
ranking(straight(A,B,C,D,E), [R]) :- value(E, V), R is 400+V.
ranking(flush(A,B,C,D,E), [R1, R2, R3, R4, R5]) :- value(E, V1), R1 is 500+V1, value(D, V2), R2 is 500+V2, value(C, V3), R3 is 500+V3, value(B, V4), R4 is 500+V4, value(A, V5), R5 is 500+V5.
ranking(fullhouse(A,B,C,D,E), [R1, R2]) :- value(A, V1), R1 is 600+V1, value(D, V2), R2 is 600+V2.
ranking(four(A,B,C,D), [R]) :- value(A, V), R is 700+V.
ranking(straightflush(A,B,C,D,E), [R]) :- value(E, V), R is 800+V.
ranking(royalflush(A,B,C,D,E), [900]).

% gets all hands of specific types
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
hands(CARDS, R) :- highs(CARDS, HIGHS), pairs(CARDS, PAIRS), twopairs(CARDS, TWOPAIRS), threes(CARDS, THREES), straights(CARDS, STRAIGHTS), flushes(CARDS, FLUSHES), fullhouses(CARDS, FULLHOUSES), fours(CARDS, FOURS), straightflushes(CARDS, STRAIGHTFLUSHES), royalflushes(CARDS, ROYALFLUSHES), append([HIGHS, PAIRS, TWOPAIRS, THREES, STRAIGHTS, FLUSHES, FULLHOUSES, FOURS, STRAIGHTFLUSHES, ROYALFLUSHES], R).

% determine whether one ranking is better than another
beats([A|TA], [B|TB]) :- A > B.
beats([A|TA], [B|TB]) :- A = B, beats(TA, TB).

% determine whether one hand is better than another
better(A, B, A) :- ranking(A, RA), ranking(B, RB), beats(RA, RB).
better(A, B, B) :- ranking(A, RA), ranking(B, RB), beats(RB, RA).
better(A, B, tie) :- ranking(A, RA), ranking(B, RB), \+ beats(RA, RB), \+ beats(RB, RA).

% find best hand for individual player.
bestof([], R, R).
bestof([H|T], C, R)  :- better(H, C, N), ((N = H; N = C), bestof(T, N, R); (N = tie, bestof(T, C, R))).

% get the best hand from a set of cards
besthand(CARDS, B) :- length(CARDS, L), L > 0, hands(CARDS, [H|T]), bestof(T, H, B).

% check who has the highest kickers
checkkickers(_, _, 0, tie).
checkkickers(C1, C2, N, p1) :- N > 0, maxrank(C1, M1), maxrank(C2, M2), value(M1, V1), value(M2, V2), V1 > V2.
checkkickers(C1, C2, N, p2) :- N > 0, maxrank(C1, M1), maxrank(C2, M2), value(M1, V1), value(M2, V2), V2 > V1.
checkkickers(C1, C2, N, R)  :- N > 0, maxrank(C1, M1), maxrank(C2, M2), value(M1, V1), value(M2, V2), V1 = V2, select(M1, C1, NC1), select(M2, C2, NC2), SUBN is N-1, checkkickers(NC1, NC2, SUBN, R).

% tiebreak between same hands
tiebreak(high(A1), high(A2), CARDSP1, CARDSP2, R) :- select(A1, CARDSP1, RP1), select(A2, CARDSP2, RP2), checkkickers(RP1, RP2, 4, R).
tiebreak(pair(A1,B1), pair(A2,B2), CARDSP1, CARDSP2, R) :- subtract(CARDSP1, [A1,B1], RP1), subtract(CARDSP2, [A2,B2], RP2), checkkickers(RP1, RP2, 3, R).
tiebreak(twopair(A1,B1,C1,D1), twopair(A2,B2,C2,D2), CARDSP1, CARDSP2, R) :- subtract(CARDSP1, [A1,B1,C1,D1], RP1), subtract(CARDSP2, [A2,B2,C2,D2], RP2), checkkickers(RP1, RP2, 1, R).
tiebreak(three(A1, B1, C1), three(A2, B2, C2), CARDSP1, CARDSP2, R) :- subtract(CARDSP1, [A1,B1,C1], RP1), subtract(CARDSP2, [A2,B2,C2], RP2), checkkickers(RP1, RP2, 2, R).
tiebreak(straight(_,_,_,_,_), straight(_,_,_,_,_), _, _, tie).
tiebreak(flush(_,_,_,_,_), flush(_,_,_,_,_), _, _, tie).
tiebreak(fullhouse(_,_,_,_,_), fullhouse(_,_,_,_,_), _, _, tie).
tiebreak(four(A1, B1, C1, D1), four(A2, B2, C2, D2), CARDSP1, CARDSP2, R) :- subtract(CARDSP1, [A1,B1,C1,D1], RP1), subtract(CARDSP2, [A2,B2,C2,D2], RP2), checkkickers(RP1, RP2, 1, R).
tiebreak(straightflush(_,_,_,_,_), straightflush(_,_,_,_,_), _, _, tie).
tiebreak(royalflush(_,_,_,_,_), royalflush(_,_,_,_,_), _, _, tie).

winner(CARDSP1, CARDSP2, p1)    :- besthand(CARDSP1, BH1), besthand(CARDSP2, BH2), better(BH1, BH2, R), (R = BH1; (R = tie, tiebreak(BH1, BH2, CARDSP1, CARDSP2, p1))).
winner(CARDSP1, CARDSP2, p2)    :- besthand(CARDSP1, BH1), besthand(CARDSP2, BH2), better(BH1, BH2, R), (R = BH2; (R = tie, tiebreak(BH1, BH2, CARDSP1, CARDSP2, p2))).
winner(CARDSP1, CARDSP2, split) :- besthand(CARDSP1, BH1), besthand(CARDSP2, BH2), better(BH1, BH2, R), tiebreak(BH1, BH2, CARDSP1, CARDSP2, T), (R = tie, T = tie).

% sample set of 7 cards with best hand as four of a kind: [card(7, h), card(7, d), card(7, s), card(7, c), card(6, h), card(6, d), card(6, s)]
