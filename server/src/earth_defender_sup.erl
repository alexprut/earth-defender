%% @private
-module(earth_defender_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor.

init([]) ->
  Procs = [
    {
      global_rooms_state,
      {global_rooms_state, start_link, []},
      permanent,
      infinity,
      worker,
      dynamic
    }
  ],
  {ok, {{one_for_one, 10, 10}, Procs}}.
