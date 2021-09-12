-module(superv).
-behavior(supervisor).

-export([start_link/0]).

%% === supervisor callbacks ===
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        {
            server,
            {server, start_link, []},
            permanent,
            10000,
            worker,
            [server]
         }
    ],
    {ok, {{one_for_one, 10, 10}, Children}}.
