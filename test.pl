:-include('poker.pl').

% to set the current games pocket: asserta(pocket(card_,_), card(_,_)).
% to set the table: retract + assert(table([card(_,_),...]))
% @< used as arbitrary ordering to remove duplicate answers

% test card sets
high_hand1([card(ace,d), card(10,h), card(5,h), card(7,s), card(9, c), card(2, s), card(queen, d)]).
high_hand2([card(king,d), card(10,h), card(5,h), card(7,s), card(9, c), card(2, s), card(queen, d)]).
high_hand3([card(ace,d), card(10,h), card(5,h), card(7,s), card(9, c), card(2, s), card(jack, d)]).
high_hand4([card(ace,d), card(8,h), card(5,h), card(7,s), card(9, c), card(2, s), card(queen, d)]).
high_hand5([card(ace,d), card(10,h), card(5,h), card(7,s), card(8, c), card(2, s), card(queen, d)]).
high_hand6([card(ace,d), card(10,h), card(5,h), card(6,s), card(8, c), card(2, s), card(queen, d)]).

pair_hand1([card(queen,d), card(10,h), card(5,h), card(7,s), card(9, c), card(2, s), card(queen, h)]).
pair_hand2([card(jack,d), card(10,h), card(5,h), card(7,s), card(9, c), card(2, s), card(jack, h)]).
pair_hand3([card(queen,d), card(9,h), card(5,h), card(7,s), card(8, c), card(2, s), card(queen, h)]).
pair_hand4([card(queen,d), card(10,h), card(5,h), card(7,s), card(8, c), card(2, s), card(queen, h)]).
pair_hand5([card(queen,d), card(10,h), card(5,h), card(6,s), card(9, c), card(2, s), card(queen, h)]).

twopair_hand1([card(queen,d), card(10,h), card(5,h), card(7,s), card(10, c), card(2, s), card(queen, h)]).
twopair_hand2([card(queen,d), card(9,h), card(5,h), card(7,s), card(9, c), card(2, s), card(queen, h)]).
twopair_hand3([card(jack,d), card(10,h), card(5,h), card(7,s), card(10, c), card(2, s), card(jack, h)]).
twopair_hand4([card(queen,d), card(10,h), card(5,h), card(6,s), card(10, c), card(2, s), card(queen, h)]).

three_hand1([card(queen,d), card(10,h), card(5,h), card(7,s), card(10, c), card(2, s), card(10, d)]).
three_hand2([card(queen,d), card(9,h), card(5,h), card(7,s), card(9, c), card(2, s), card(9, d)]).
three_hand3([card(jack,d), card(10,h), card(5,h), card(7,s), card(10, c), card(2, s), card(10, d)]).
three_hand4([card(queen,d), card(10,h), card(5,h), card(6,s), card(10, c), card(2, s), card(10, d)]).

straight_hand1([card(10,d), card(ace,h), card(9,h), card(7,s), card(3, s), card(jack, s), card(8, d)]).
straight_hand2([card(10,d), card(ace,h), card(9,h), card(7,s), card(3, s), card(6, s), card(8, d)]).

flush_hand1([card(ace,d), card(10,d), card(5,d), card(7,d), card(9, c), card(2, s), card(queen, d)]).
flush_hand2([card(king,d), card(10,d), card(5,d), card(7,d), card(9, c), card(2, s), card(queen, d)]).

fullhouse_hand1([card(ace,d), card(ace,h), card(5,h), card(5,s), card(ace, s), card(king, s), card(queen, d)]).
fullhouse_hand2([card(ace,d), card(ace,h), card(6,h), card(6,s), card(ace, s), card(king, s), card(queen, d)]).
fullhouse_hand3([card(king,d), card(king,h), card(5,h), card(5,s), card(ace, s), card(king, s), card(queen, d)]).
fullhouse_hand4([card(ace,d), card(ace,h), card(5,h), card(5,s), card(ace, s), card(jack, s), card(queen, d)]).

four_hand1([card(ace,d), card(ace,h), card(5,h), card(ace,c), card(ace, s), card(king, s), card(3, d)]).
four_hand2([card(queen,d), card(queen,h), card(5,h), card(queen,c), card(queen, s), card(king, s), card(3, d)]).
four_hand3([card(ace,d), card(ace,h), card(5,h), card(ace,c), card(ace, s), card(queen, s), card(3, d)]).

straightflush_hand1([card(king,d), card(queen,d), card(jack,d), card(10,d), card(9, d), card(king, s), card(queen, s)]).
straightflush_hand2([card(8,d), card(queen,d), card(jack,d), card(10,d), card(9, d), card(king, s), card(queen, s)]).

royalflush_hand1([card(ace,d), card(king,d), card(queen,d), card(jack,d), card(10, d), card(king, s), card(queen, d)]).
royalflush_hand2([card(ace,c), card(king,c), card(queen,c), card(jack,c), card(10, c), card(king, s), card(queen, d)]).




:- begin_tests(poker).

test('besthand - high', [nondet])                       :- high_hand1(CARDS), besthand(CARDS, high(card(ace,d))).
test('winner - high vs worse high by value', [nondet])  :- high_hand1(CARDS1), high_hand2(CARDS2), winner(CARDS1, CARDS2, p1).
test('winner - high vs worse high by kicker', [nondet]) :- high_hand1(CARDS1), high_hand3(CARDS2), winner(CARDS1, CARDS2, p1).
test('winner - high vs worse high by kicker', [nondet]) :- high_hand1(CARDS1), high_hand4(CARDS2), winner(CARDS1, CARDS2, p1).
test('winner - high vs worse high by kicker', [nondet]) :- high_hand1(CARDS1), high_hand5(CARDS2), winner(CARDS1, CARDS2, p1).
test('winner - high vs worse high by kicker', [nondet]) :- high_hand1(CARDS1), high_hand6(CARDS2), winner(CARDS1, CARDS2, p1).

test('besthand - pair', [nondet])                       :- pair_hand1(CARDS), besthand(CARDS, pair(card(queen,d), card(queen,h))).
test('winner - pair vs worse hand', [nondet])           :- pair_hand1(CARDS1), high_hand1(CARDS2), winner(CARDS1, CARDS2, p1).
test('winner - pair vs worse pair by value', [nondet])  :- pair_hand1(CARDS1), pair_hand2(CARDS2), winner(CARDS1, CARDS2, p1).
test('winner - pair vs worse pair by kicker', [nondet]) :- pair_hand1(CARDS1), pair_hand3(CARDS2), winner(CARDS1, CARDS2, p1).
test('winner - pair vs worse pair by kicker', [nondet]) :- pair_hand1(CARDS1), pair_hand4(CARDS2), winner(CARDS1, CARDS2, p1).
test('winner - pair vs worse pair by kicker', [nondet]) :- pair_hand1(CARDS1), pair_hand5(CARDS2), winner(CARDS1, CARDS2, p1).

test('besthand - twopair', [nondet])                                      :- twopair_hand1(CARDS), besthand(CARDS, twopair(card(10,c), card(10,h), card(queen,d), card(queen,h))).
test('winner - twopair vs worse hand', [nondet])                          :- twopair_hand1(CARDS1), pair_hand1(CARDS2), winner(CARDS1, CARDS2, p1).
test('winner - twopair vs worse twopair by value of high pair', [nondet]) :- twopair_hand1(CARDS1), twopair_hand3(CARDS2), winner(CARDS1, CARDS2, p1).
test('winner - twopair vs worse twopair by value of low pair', [nondet])  :- twopair_hand1(CARDS1), twopair_hand2(CARDS2), winner(CARDS1, CARDS2, p1).
test('winner - twopair vs worse twopair by kicker', [nondet])             :- twopair_hand1(CARDS1), twopair_hand4(CARDS2), winner(CARDS1, CARDS2, p1).

test('besthand - three', [nondet])                        :- three_hand1(CARDS), besthand(CARDS, three(card(10,c), card(10,d), card(10,h))).
test('winner - three vs worse hand', [nondet])            :- three_hand1(CARDS1), twopair_hand1(CARDS2), winner(CARDS1, CARDS2, p1).
test('winner - three vs worse three by value', [nondet])  :- three_hand1(CARDS1), three_hand2(CARDS2), winner(CARDS1, CARDS2, p1).
test('winner - three vs worse three by kicker', [nondet]) :- three_hand1(CARDS1), three_hand3(CARDS2), winner(CARDS1, CARDS2, p1).
test('winner - three vs worse three by kicker', [nondet]) :- three_hand1(CARDS1), three_hand4(CARDS2), winner(CARDS1, CARDS2, p1).

test('besthand - straight', [nondet])                 :- straight_hand1(CARDS), besthand(CARDS, straight(card(7,s), card(8,d), card(9,h), card(10,d), card(jack,s))).
test('winner - straight vs worse hand', [nondet])     :- straight_hand1(CARDS1), three_hand1(CARDS2), winner(CARDS1, CARDS2, p1).
test('winner - straight vs worse straight', [nondet]) :- straight_hand1(CARDS1), straight_hand2(CARDS2), winner(CARDS1, CARDS2, p1).

test('besthand - flush', [nondet])                 :- flush_hand1(CARDS), besthand(CARDS, flush(card(5,d), card(7,d), card(10,d), card(queen,d), card(ace,d))).
test('winner - flush vs worse hand', [nondet])     :- flush_hand1(CARDS1), straight_hand1(CARDS2), winner(CARDS1, CARDS2, p1).
test('winner - flush vs worse straight', [nondet]) :- flush_hand1(CARDS1), flush_hand2(CARDS2), winner(CARDS1, CARDS2, p1).

test('besthand - fullhouse', [nondet])                            :- fullhouse_hand1(CARDS), besthand(CARDS, fullhouse(card(ace,d), card(ace,h), card(ace,s), card(5,h), card(5, s))).
test('winner - fullhouse vs worse hand', [nondet])                :- fullhouse_hand1(CARDS1), flush_hand1(CARDS2), winner(CARDS1, CARDS2, p1).
test('winner - fullhouse vs better hand', [nondet])               :- fullhouse_hand1(CARDS1), four_hand1(CARDS2), winner(CARDS1, CARDS2, p2).
test('winner - fullhouse vs better fullhouse by pair', [nondet])  :- fullhouse_hand1(CARDS1), fullhouse_hand2(CARDS2), winner(CARDS1, CARDS2, p2).
test('winner - fullhouse vs better fullhouse by three', [nondet]) :- fullhouse_hand3(CARDS1), fullhouse_hand1(CARDS2), winner(CARDS1, CARDS2, p2).
test('winner - fullhouse vs equal fullhouse split', [nondet])     :- fullhouse_hand4(CARDS1), fullhouse_hand1(CARDS2), winner(CARDS1, CARDS2, split).

test('besthand - four', [nondet])                       :- four_hand1(CARDS), besthand(CARDS, four(card(ace,c), card(ace,d), card(ace,h), card(ace,s))).
test('winner - four vs worse hand', [nondet])           :- four_hand1(CARDS1), fullhouse_hand1(CARDS2), winner(CARDS1, CARDS2, p1).
test('winner - four vs worse four by value', [nondet])  :- four_hand1(CARDS1), four_hand2(CARDS2), winner(CARDS1, CARDS2, p1).
test('winner - four vs worse four by kicker', [nondet]) :- four_hand1(CARDS1), four_hand3(CARDS2), winner(CARDS1, CARDS2, p1).

test('besthand - straightflush', [nondet])                      :- straightflush_hand1(CARDS), besthand(CARDS, straightflush(card(9,d), card(10,d), card(jack,d), card(queen,d), card(king,d))).
test('winner - straightflush vs worse hand', [nondet])          :- straightflush_hand1(CARDS1), four_hand1(CARDS2), winner(CARDS1, CARDS2, p1).
test('winner - straightflush vs worse straightflush', [nondet]) :- straightflush_hand1(CARDS1), straightflush_hand2(CARDS2), winner(CARDS1, CARDS2, p1).

test('besthand - royalflush', [nondet])                   :- royalflush_hand1(CARDS), besthand(CARDS, royalflush(card(ace,d), card(king,d), card(queen,d), card(jack,d), card(10, d))).
test('winner - royalflush vs worse hand', [nondet])       :- royalflush_hand1(CARDS1), straight_hand1(CARDS2), winner(CARDS1, CARDS2, p1).
test('winner - royalflush vs equal royalflush', [nondet]) :- royalflush_hand1(CARDS1), royalflush_hand2(CARDS2), winner(CARDS1, CARDS2, split).

:- end_tests(poker).
