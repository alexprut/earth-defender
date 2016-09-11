-module(earth_defender_app).

-behaviour(application).

%% External exports
-export([]).

%% Internal exports
-export([start/2, stop/1]).

-include("config.hrl").

%%% ---------------------------------------------------
%%%
%%% Application.
%%%
%%% ---------------------------------------------------

start(_Type, _Args) ->
  Dispatch = cowboy_router:compile([
    {'_', [{"/websocket/", websocket_handler, []}]}
  ]),
  {ok, _} = cowboy:start_clear(http, 100, [{port, utils:get_port()}], #{
    env => #{dispatch => Dispatch}
  }),
  earth_defender_sup:start_link().

stop(_State) ->
  ok.
