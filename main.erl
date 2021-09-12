-module(main).

-export([start/0, stop/0]).
-export([init/0]).
-export([deposit/2]).

start() ->
    io:format("---> Starting program...~n"),
    Pid = spawn(main, init, []),
    register(main, Pid).

init() ->
    Accounts = dict:new(),
    loop(Accounts).

stop() ->
    main ! terminate.

deposit(AccID, Amt) ->
    main ! {deposit, AccID, Amt}.

loop(Accounts) ->
    receive
        {deposit, AccID, Amt} ->
            CurrentBalance = case dict:find(AccID, Accounts) of
                                 error -> 0;
                                 {ok, Amt0} -> Amt0
                             end,
            Accounts1 = dict:store(
                            AccID,
                            CurrentBalance + Amt,
                            Accounts
                        ),
            io:format("---> ~p deposited ~p.~n", [AccID, Amt]),
            loop(Accounts1);
        terminate -> io:format("---> Exiting program...~n")
    end.
