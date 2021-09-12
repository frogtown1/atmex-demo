%% @author Benjamin McGriff <mcgriff.benjamin@gmail.com>

-module(server).
-behavior(gen_server).

%% ===== gen_server callbacks =====
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).



-export([start/0, stop/0]).
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
