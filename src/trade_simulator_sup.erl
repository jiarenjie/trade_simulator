%%%-------------------------------------------------------------------
%% @doc trade_simulator top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(trade_simulator_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(MPAYSUP, pay_sup).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    MPAYSUP = {?MPAYSUP,
        {?MPAYSUP, start_link, []},
        permanent, 2000, supervisor, [?MPAYSUP]},
    {ok, { {one_for_one, 2, 60}, [MPAYSUP]} }.

%%====================================================================
%% Internal functions
%%====================================================================
