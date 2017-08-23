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
-define(MSIMULATORSTORE, simulator_store).
-define(MUPSUP, up_sup).
-define(MSIMULATORWEB, simulator_web).
-define(MSIMULATORWENV, simulator_env).
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
    MSIMULATORSTORE = {?MSIMULATORSTORE,
        {?MSIMULATORSTORE, start_link, []},
        permanent, 2000, supervisor, [?MSIMULATORSTORE]},
    UPSUP = {?MUPSUP,
        {?MUPSUP, start_link, []},
        permanent, 2000, supervisor, [?MUPSUP]},

    SIMULATORWEB = {?MSIMULATORWEB,
        {?MSIMULATORWEB, start_link, []},
        permanent, 2000, supervisor, [?MSIMULATORWEB]},

    SIMULATORWENV = {?MSIMULATORWENV,
        {?MSIMULATORWENV, start_link, []},
        permanent, 2000, supervisor, [?MSIMULATORWENV]},


    {ok, { {one_for_one, 2, 60}, [MPAYSUP,MSIMULATORSTORE,UPSUP,SIMULATORWEB,SIMULATORWENV]} }.

%%====================================================================
%% Internal functions
%%====================================================================
