-module(exercise2).
-export([first_thread/0, second_thread/1]).


%%  create second thread (monitored), pass self() as argument so it knows how to reply
%%  exchange messages with second thread
%%  restart second thread when it dies
first_thread() ->
    { SecondThread, _ } = spawn_monitor(?MODULE, second_thread, [self()]),
    io:fwrite("second thread is ~p~n", [SecondThread]),
    first_thread_loop(SecondThread).


%%  the repeat actions of first thread:
%%  get 2 ints, send them, receive & print sum, exit if sum=0, repeat
first_thread_loop(Partner) ->
    {ok, [ X1, X2 ]} = io:fread("enter two integers please> ", "~d~d"),
    Partner ! {X1, X2},
    receive
        { 'DOWN', _, process, _, _ } ->
	    { NewThread, _ } = spawn_monitor(?MODULE, second_thread, [self()]),
	    io:fwrite("restart second thread; new thread is ~p~n", [NewThread]),
	    SecondThread = NewThread;
        Sum ->
            io:fwrite("~p + ~p = ~p~n", [ X1, X2, Sum ]),
	    SecondThread = Partner
    end,
    first_thread_loop(SecondThread).
   


%%  second thread: receive 2 ints, send sum (unless sum=0), repeat
%%  if sum=0 then die
second_thread(Partner) ->
    receive
        {Num1, Num2} ->
            Sum = Num1 + Num2,
            io:fwrite("thread ~p: ~p + ~p = ~p~n", [ self(), Num1, Num2, Sum ]),
	    if
	        Sum == 0 ->
	            exit(sum_is_zero);
		true ->
	            Partner ! Sum
	    end
    end,
    second_thread(Partner).
