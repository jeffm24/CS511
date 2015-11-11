-module(exercise1).
-export([first_thread/0, second_thread/1]).

%%
%%
first_thread() ->
    SecondThread = spawn(?MODULE, second_thread, [self()]),
    first_thread_loop(SecondThread).

%%
%%
first_thread_loop(Partner) ->
    {ok, [ X1, X2 ]} = io:fread("enter two integers please> ", "~d~d"),
    Partner ! {X1, X2},
    receive
        0 ->
            io:fwrite("~p + ~p = 0~n", [ X1, X2 ]),
            halt();
        Sum ->
            io.fwrite("~p + ~p = ~p~n", [ X1, X2, Sum ]),
            first_thread_loop(Partner)
    end.

    %%
    second_thread(Partner) ->
        receive
            {Num1, Num2} ->
                Sum = Num1 + Num2,
                Partner ! Sum
