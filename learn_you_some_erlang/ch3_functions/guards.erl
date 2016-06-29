-module(guards).
-compile(export_all). % lazy; should use -export()

old_enough(X) when X >= 16 -> true;
old_enough(_)              -> false.

right_age(X) when 16 =< X, X =< 104 -> true;
right_age(_)                        -> false.
