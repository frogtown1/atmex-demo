%% @author Benjamin McGriff <mcgriff.benjamin@gmail.com>

-module(server).
-behavior(gen_server).

%% ===== gen_server callbacks =====
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {
            accounts
        }).

%% API functions
start_link() ->
    io:format("---> Starting program.~n"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE).

deposit(AccountId, Amount) ->
    gen_server:call(?MODULE, {deposit, AccountId, Amount}).

balance(AccountId) ->
    gen_server:call(?MODULE, {balance, AccountId}).

withdraw(AccountId, Amount) ->
    gen_server:call(?MODULE, {withdraw, AccountId, Amount}).

%% ====== Internals =====
init([]) ->
  Accounts = dict:new(),
  State = #state{accounts = Accounts},
  {ok, State}.

handle_call({deposit, AccountId, Amount}, _From, #state{
                                                     accounts = Accounts
                                                 } = State ->
                                                     CurrentBalance = get_current_balance(AccountId, Accounts),
                                                     Accounts1 = dict:store(
                                                                     AccountId,
                                                                     CurentBalance + Amount,
                                                                     Accounts
                                                                 ),
                                                     {reply, ok, State#state{accounts = Accounts1}};

handle_call({balance, AccountId}, _From, #state{
                                             accounts = Accounts
                                         } = State) ->
                                             CurrentBalance = get_current_balance(AccountId, Accounts),
                                             {reply, CurrentBalance, State};

handle_call({withdraw, AccountId, Amount}, _From, #state{
                                                      accounts = Accounts
                                                  } = State) ->
                                                      case get_current_balance(AccountId, Accounts) of
                                                          CurrentBalance when Amount =< CurrentBalance ->
                                                              Accounts1 = dict:store(
                                                                              AccountId,
                                                                              CurrentBalance - Amount,
                                                                              Accounts
                                                                          ),
                                                                          {reply, ok, State#state{accounts = Accounts1}};
                                                          _-> {reply, {error, insufficient_funds}, State}
                                                      end;

%% Prevent mem leaks from error msg overflows
handle_call(_Msg, _From, State) ->
    {reply, undefined, State}.


