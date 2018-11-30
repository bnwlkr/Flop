:- use_module(library(lists)).

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
maxrank([H|T], R) :- maxrank(T, R), greater(R, H).
maxrank([H|T], H) :- maxrank(T, R), greater(H, R).

greater(card(V1, S1), card(V2, S2)) :- order(V1, OV1), order(V2, OV2), ((OV1 > OV2); (OV1 = OV2, suitvalue(S1, SV1), suitvalue(S2, SV2), SV1 > SV2)).

% for straights
order(X, V):- value(card(X,_), V).

% hands
royalflush(card(ace, S), card(king, S), card(queen, S), card(jack, S), card(10, S)).
straightflush(card(A,S),card(B,S),card(C,S),card(D,S),card(E,S)) :- order(A, OA),order(B, OB),order(C, OC),order(D, OD),order(E, OE), OA is OB-1, OB is OC-1, OC is OD-1, OD is OE-1.
four(card(A,SW), card(A,SX), card(A,SY), card(A,SZ)) :- SW @< SX, SX @< SY, SY @< SZ.
fullhouse(card(A,AS1), card(A,AS2), card(A,AS3), card(B,BS1), card(B,BS2)) :- AS1 @< AS2, AS2 @< AS3, BS1 @< BS2, dif(A,B).
flush(card(A,S), card(B,S), card(C,S), card(D,S), card(E,S)) :- order(A, AV),order(B, BV),order(C, CV),order(D, DV),order(E, EV), AV @< BV, BV @< CV, CV @< DV, DV @< EV.
straight(A,B,C,D,E) :- value(A, OA),value(B, OB),value(C, OC),value(D, OD),value(E, OE), OA is OB-1, OB is OC-1, OC is OD-1, OD is OE-1, \+ straightflush(A,B,C,D,E).
three(card(A,SX), card(A, SY), card(A, SZ)) :- SX @< SY, SY @< SZ.
twopair(A,B,C,D) :- pair(A,B), pair(C,D), A @< C.
pair(card(A,SX), card(A,SY)) :- SX @< SY.
high(card(_,_)).

% hand ranks
ranking(high(A), [R]) :- value(A, R).
ranking(pair(A,_), [R]) :- value(A, V), R is 100+V.
ranking(twopair(A,_,C,_), [R1, R2]) :- value(C, V1), R1 is 200+V1, value(A, V2), R2 is 200+V2.
ranking(three(A,_,_), [R]) :- value(A, V), R is 300+V.
ranking(straight(_,_,_,_,E), [R]) :- value(E, V), R is 400+V.
ranking(flush(A,B,C,D,E), [R1, R2, R3, R4, R5]) :- value(E, V1), R1 is 500+V1, value(D, V2), R2 is 500+V2, value(C, V3), R3 is 500+V3, value(B, V4), R4 is 500+V4, value(A, V5), R5 is 500+V5.
ranking(fullhouse(A,_,_,D,_), [R1, R2]) :- value(A, V1), R1 is 600+V1, value(D, V2), R2 is 600+V2.
ranking(four(A,_,_,_), [R]) :- value(A, V), R is 700+V.
ranking(straightflush(_,_,_,_,E), [R]) :- value(E, V), R is 800+V.
ranking(royalflush(_,_,_,_,_), [900]).

% gets all hands of specific types
highs(CARDS, high(R)) :-
    maxrank(CARDS, R).

pairs(CARDS, R) :-
    maxrank(CARDS, M), select(M, CARDS, NCARDS),
    ((value(M, V), member(card(V, S), NCARDS), R = pair(V, card(M, S))); !;
    pairs(NCARDS, R)).

twopairs(CARDS, twopair(NC1, NC2, C1, C2)) :-
    pairs(CARDS, pair(C1, C2)), subtract(CARDS, [C1, C2], NCARDS), pairs(NCARDS, NC1, NC2).

threes(CARDS, R) :-
    maxrank(CARDS, M), select(M, CARDS, NCARDS),
    ((value(M, V), member(card(V, S1), NCARDS), member(card(V, S2), NCARDS), dif(S1, S2), R = three(M, card(V, S1), card(V, S2))); !;
    threes(NCARDS, R)).

straights(CARDS, R) :-
    maxrank(CARDS, M), value(M, V1),
    ((V2 is V1-1, V3 is V2-1, V4 is V3-1, V5 is V4-1,
        value(card(R2, _), V2), value(card(R3, _), V3), value(card(R4, _), V4), value(card(R5, _), V5),
        subset([card(R2, S2), card(R3, S3), card(R4, S4), card(R5, S5)], CARDS),
        R = straight(card(R5, S5), card(R4, S4), card(R3, S3), card(R2, S2), M));
    (select(M, CARDS, NCARDS), straights(NCARDS, R))).

flushes(CARDS, R) :-
    maxrank(CARDS, card(V, S)), select(card(V, S), CARDS, NCARDS), findall(card(X, S), (member(card(X, S), NCARDS)), SUITED), length(SUITED, L),
    ((L > 3); !;
    (L < 4, flushes(NCARDS, R))).

fullhouses(CARDS, R) :-
    threes(CARDS, three(C1, C2, C3)), subtract(CARDS, [C1, C2, C3], NCARDS),
    pairs(NCARDS, pair(C4, C5)), R = fullhouse(C1, C2, C3, C4, C5).

fours(CARDS, R) :-
    maxrank(CARDS, M), select(M, CARDS, NCARDS),
    ((value(M, V), subset([card(V, S1), card(V, S2), card(V, S3)], NCARDS),
        dif(S1, S2), dif(S2, S3), dif(S3, S1), R = four(M, card(V, S1), card(V, S2), card(V, S3))); !;
    fours(NCARDS, R)).

straightflushes(CARDS, R) :-
    straights(CARDS, straight(A, B, C, D, E)),
    ((flush(A, B, C, D, E), R = straightflush(A, B, C, D, E));
    (select(E, CARDS, NCARDS), straightflushes(NCARDS, R))).

royalflushes(CARDS, royalflush(card(ace, S), card(king, S), card(queen, S), card(jack, S), card(10, S))) :-
    subset([card(ace, S), card(king, S), card(queen, S), card(jack, S), card(10, S)], CARDS).

% generate all hands
hands(CARDS, R) :- hands(CARDS, royalflushes, R).
hands(CARDS, royalflushes, R)    :- royalflushes(CARDS, R);    (\+ royalflushes(CARDS, _),    hands(CARDS, straightflushes, R)).
hands(CARDS, straightflushes, R) :- straightflushes(CARDS, R); (\+ straightflushes(CARDS, _), hands(CARDS, fours, R)).
hands(CARDS, fours, R)           :- fours(CARDS, R);           (\+ fours(CARDS, _),           hands(CARDS, fullhouses, R)).
hands(CARDS, fullhouses, R)      :- fullhouses(CARDS, R);      (\+ fullhouses(CARDS, _),      hands(CARDS, flushes, R)).
hands(CARDS, flushes, R)         :- flushes(CARDS, R);         (\+ flushes(CARDS, _),         hands(CARDS, straights, R)).
hands(CARDS, straights, R)       :- straights(CARDS, R);       (\+ straights(CARDS, _),       hands(CARDS, threes, R)).
hands(CARDS, threes, R)          :- threes(CARDS, R);          (\+ threes(CARDS, _),          hands(CARDS, twopairs, R)).
hands(CARDS, twopairs, R)        :- twopairs(CARDS, R);        (\+ twopairs(CARDS, _),        hands(CARDS, pairs, R)).
hands(CARDS, pairs, R)           :- pairs(CARDS, R);           (\+ pairs(CARDS, _),           hands(CARDS, highs, R)).
hands(CARDS, highs, R)           :- highs(CARDS, R).

% determine whether one ranking is better than another
beats([A|_], [B|_]) :- A > B.
beats([A|TA], [B|TB]) :- A = B, beats(TA, TB).

% determine whether one hand is better than another
better(A, B, A) :- ranking(A, RA), ranking(B, RB), beats(RA, RB).
better(A, B, B) :- ranking(A, RA), ranking(B, RB), beats(RB, RA).
better(A, B, tie) :- ranking(A, RA), ranking(B, RB), \+ beats(RA, RB), \+ beats(RB, RA).

% get the best hand from a set of cards
besthand(CARDS, R) :- length(CARDS, L), L > 0, hands(CARDS, R).

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
