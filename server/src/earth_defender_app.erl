%% @private
-module(earth_defender_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

-include("config.hrl").

%% API.
start(_Type, _Args) ->
  case init:get_argument(port) of
    error ->
      Websocket_port = ?WEBSOCKET_PORT;
    {_, [[Port]]} ->
      {Websocket_port, []} = string:to_integer(Port)
  end,
  Dispatch = cowboy_router:compile([
    {'_', [{"/websocket/", websocket_handler, []}]}
  ]),
  {ok, _} = cowboy:start_clear(http, 100, [{port, Websocket_port}], #{
    env => #{dispatch => Dispatch}
  }),
  case init:get_argument(role) of
    error ->
      ok;
    _ ->
      {_, [[Master_name]]} = init:get_argument(master_name),
      slave_handler:connect_to_master(erlang:list_to_atom(Master_name))
  end,
  earth_defender_sup:start_link().

stop(_State) ->
  ok.
