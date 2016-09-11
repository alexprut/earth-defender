-module(earth_defender_sup).

-behaviour(supervisor).

%% External exports
-export([start_link/0]).

%% Internal exports
-export([init/1]).

%%% ---------------------------------------------------
%%%
%%% Supervisor.
%%%
%%% ---------------------------------------------------

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
  Procs = [
    {
      local_rooms_state,
      {local_rooms_state, start_link, []},
      permanent,
      infinity,
      worker,
      dynamic
    }
      
  ],
  {ok, {{one_for_one, 10, 10}, Procs}}.
