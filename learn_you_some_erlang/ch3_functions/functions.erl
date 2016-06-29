-module(functions).
-compile(export_all). % lazy; should use -export()

head([H|_]) -> H.

second([_,X|_]) -> X.

same(X,X) -> true;
same(_,_) -> false.


