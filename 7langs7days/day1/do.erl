-module(do).
-export([words/1]).
-export([words_tr/1]).
-export([count_to/1]).
-export([status/1]).

% Exercises ("Do") from
% Bruce A. Tate, 7 Languages in 7 weeks, 2010, p. 173
%
% 1. write a function that uses recursion to return
%    the number of words in a string, i.e. implement
%    string:words/1
% 2. Write a function that uses recursion to count to ten
% 3. Write a function that uses matching to selectively
%    print "success" or "error: message" given input of
%    the form {error, Message} or sucess.

%
% Solutions
%

% 1. my naive solution
words("")     -> 0;
words(String) ->
  List = string:tokens(String, " "),
  1 + words(string:join(tl(List), " ")).

% 1. using tail recursion viz. Fred Hebert, Learn you
%    Some Erlang for Great Good, 2013, p. 65

% abstract away tail recursion arity 2 to a simpler
% arity 1 interface.  better ui for the user.
words_tr(String)      -> words_tr(String, 0).

% base case
% use the accumulator pattern, much like Ruby
% inject (alias reduce).  I think in this case
% the reduce paradigm is appropriate: take a string of
% words and reduce it to an integer.
words_tr("", Acc)     -> Acc;
% general case
words_tr(String, Acc) ->
  List = string:tokens(String, " "),
  words_tr(string:join(tl(List), " "), Acc+1).


% 2. my solution using tail recursion
%    I interpreted the problem to mean, recursively
%    generate the list [1,2,3,4,5,6,7,8,9,10]

% here i'm using the accumulator pattern again,
% but this time i'm using a list as the accumulator.
% Again, much like Ruby inject, or should it be
% reduce?  I have an integer that I want to use
% to build a list.  I'm taking a thing and reducing
% it down to 1 list.  Meh.
count_to(N)       -> count_to(N,[]).

count_to(0, Acc)  -> Acc;
count_to(N, Acc)  -> count_to(N-1, [N|Acc]).


% 3. my solution
status(success)          -> "success";
status({error, Message}) -> string:join(["error:", Message], " ").
