-module(atmex).

-export([start/0, stop/0]).
-export([deposit/2]).
-export([balance/1]).
-export([withdraw/2]).

start() ->
    application:start(atmex).

stop() ->
    application:stop(atmex).

deposit(AccountId, Amount) ->
    atmex_gens:deposit(AccountId, Amount).

balance(AccountId) ->
    atmex_gens:balance(AccountId).

withdraw(AccountId, Amount) ->
    atmex_gens:withdraw(AccountId, Amount).
