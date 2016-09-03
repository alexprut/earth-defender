%% @private
-module(earth_defender_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

-include("config.hrl").

%% API.
start(_Type, _Args) ->
  Dispatch = cowboy_router:compile([
    {'_', [{"/websocket/", websocket_handler, []}]}
  ]),
  {ok, _} = cowboy:start_clear(http, 100, [{port, utils:get_port()}], #{
    env => #{dispatch => Dispatch}
  }),
  case utils:get_role() of
    master ->
      ok;
    slave ->
      slave_handler:connect_to_master(utils:get_master_name())
  end,
  earth_defender_sup:start_link().

stop(_State) ->
  ok.
