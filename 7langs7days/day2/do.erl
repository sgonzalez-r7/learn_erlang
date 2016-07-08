-module(do).
-export([select/2]).
-export([total_price/1]).
-export([winner/1, winner/2]).
-compile([export_all]).
% Bruce A. Tate, Seven Languages in Seven Weeks, ch. 6,
% day 2 do

% 1. Consider a list of keyword-value tuples, such as
%    [{erlang, "a functional language"}, {ruby,
%     "an OO language"}]. Write a function that accepts
%    the list and a keyword and returns the associated
%    value for the keyword
select(_, []) ->
  "Not hits. Your list is empty.";
select(Key, List) ->
  Hit  = fun({K, _}) -> K == Key end,
  Hits = lists:filter(Hit, List),
  case Hits of
    [] ->
      "No hits. No entry with Key: " ++ atom_to_list(Key);
    [{Key, Value}|_] ->
      Value
  end.



% 2. Consider a shopping list that looks like [{item,
%    quantity,price}, ...].  Write a list comprehension
%    that builds a list of items of the form [{item,
%    total_price}, ...], where total_price is quantity
%    times price
total_price(Items) ->
  [{I, Q*P} || {I, Q, P} <- Items].


% Bonus Problem
% 3. Write a program that reads a tic-tac-toe board
%    presented as a list or a tuple of size nine. Return
%    the winner (x or o) if a winner has been determined,
%    cat if there are no more possible moves, or
%    no_winner if no player has won yet.
winner(x, Board) when score(x, Board) == 15 -> x;
winner(o, Board) when score(o, Board) == 15 -> y.

score(Player, Board) ->
  Weights = [8,1,6,3,5,7,4,9,2],
  Hash = lists:zip(Board, Weights),
  Hit = fun({P,_W}) -> P == Player end,
  Hits  = lists:filter(Hit, Hash),
  Add = fun({_P,V}, Sum) -> Sum + V end,
  Score = lists:foldl(Add, 0, Hits),
  Score.
















