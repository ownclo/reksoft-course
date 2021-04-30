-module(divide_by_zero).
-compile([export_all]).

% erlang can tell you if the expression
% always fail with badarith.
test() -> 1/0.
