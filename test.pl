:-include('poker.pl').

% to set the current games pocket: asserta(pocket(card_,_), card(_,_)).
% to set the table: retract + assert(table([card(_,_),...]))
% @< used as arbitrary ordering to remove duplicate answers

% test card sets
fullhouse_hand([card(ace,d), card(ace,h), card(5,h), card(5,s), card(ace, s), card(king, s), card(queen, d)]).



:- begin_tests(poker).

test('fullhouse', [nondet]) :- fullhouse_hand(H), hands(H, HANDS), best(HANDS, fullhouse(card(ace,d), card(ace,h), card(ace,s), card(5,h), card(5, s))).

:- end_tests(poker).



