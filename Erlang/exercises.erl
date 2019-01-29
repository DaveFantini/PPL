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



% Define the function for an applier actor, which has a state S, holding a value, and receives a function f from other actors: if S =f(S),
% it sends back the result S and ends it computation; otherwise sends back a message to state that the condition S = f(S) has not
% been reached

applier(S) ->
    receive
        {F, Pid} -> case
                        F(S) == S -> Pid ! S;
                        true -> Pid ! {error, F(S)},
                                applier(F(S))
                    end
    end. 


fix(IV, F) ->
    App = spawn(?MODULE, applier, [IV]),
    loop(F, App).
    
loop(F, App) ->
    App ! {F, self()},
    receive
        {error, _} -> loop(F, App);
        {V} -> io:format("fixed point value  ~p", [V])
    end. 
