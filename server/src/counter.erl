-module(counter).

-export([inc/0,dec/0,current/0,start/0,loop/1]).

inc() ->
    global_counter ! inc_counter,
    ok.

dec() ->
    global_counter ! dec_counter,
    ok.

current() ->
    global_counter ! {get_counter, self()},
    receive
        { counter_value, Cnt } ->
            integer_to_list(Cnt)
    end.
       
start() ->
    register(global_counter,spawn(counter,loop,[0])),
    ok.

loop(Counter) ->
    receive
        inc_counter ->
            NewCounter = Counter+1;
        dec_counter ->
            NewCounter = Counter-1;
        {get_counter, Pid} ->
            Pid ! { counter_value, Counter },
            NewCounter = Counter
    end,
    loop(NewCounter).
