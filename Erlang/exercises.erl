-module(exercises).
-export([go/0, process/2]).

process (Beh, Creator) ->
    receive
        {load, F, Creator} ->
            process ( fun (X) -> Beh(F(X)) end , Creator);
        {run, D, Creator} ->
            Creator ! {self(), Beh(D)},
            process(Beh, Creator);
        {stop, Creator} ->
            true
    end.

go() ->
    Proc = spawn (?MODULE, process, [(fun (X) -> X+2 end), self()]),
    Proc ! {load, (fun(X) -> X*2 end), self()},
    Proc ! {run, 5, self()},
    receive
        {Proc, N} ->
            io:format("result ~p", [N])
    end. 