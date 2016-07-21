-module(do).
-export([select/2]).
-export([total_price/1]).
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
score(B =[B1,B2,B3, B4,B5,B6, B7,B8,B9]) ->
  P = [ 0, 1, 2,  3, 4, 5,  6, 7, 8],
  W = [ 8, 1, 6,  3, 5, 7,  4, 9, 2],
  Board = lists:zip3(B, P, W),

  io:format("~p\n", [Board]),

  Row_posistions    = [{0,1,2}, {3,4,5}, {6,7,8}],
  Column_posistions = [{0,3,6}, {1,4,7}, {2,5,8}],
  Diag_posistions   = [{0,4,8}, {2,4,6},

  Rows = lists:filter(fun({M,P,W}) ->  end, Board),


  io:format("rows  = ~p\n", [Rows]).
  % io:format("cols  = ~p\n", [Cols]),
  % io:format("diags = ~p\n", [Diags]).

  % [{x, 15}, {o, }]







    % = [ x, x, x,  e, o, e,  e, o, o],










