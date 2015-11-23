-module(exercise1).
-export([first_thread/0, second_thread/1]).     % must also export function that will be spawned


%%  create second thread, pass self() as argument so it knows how to reply
%%  exchange messages with second thread until sum=0
first_thread() ->
    SecondThread = spawn(?MODULE, second_thread, [self()]),
    first_thread_loop(SecondThread).


%%  the repeat actions of first thread:
%%  get 2 ints, send them, receive & print sum, exit if sum=0, repeat
first_thread_loop(Partner) ->
    {ok, [ X1, X2 ]} = io:fread("enter two integers please> ", "~d~d"),
    Partner ! {X1, X2},
    receive
        0 ->
            io:fwrite("~p + ~p = 0~n", [ X1, X2 ]),
            halt();
        Sum ->
            io:fwrite("~p + ~p = ~p~n", [ X1, X2, Sum ]),
            first_thread_loop(Partner)
    end.


%%  second thread: receive 2 ints, send sum, repeat
second_thread(Partner) ->
    receive
        {Num1, Num2} ->
            Sum = Num1 + Num2,
	    Partner ! Sum;
        _ ->
            io:fwrite("second_thread: received unexpected message~n", [])
    end,
    second_thread(Partner).
