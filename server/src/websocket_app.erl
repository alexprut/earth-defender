%% @private
-module(websocket_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

-include("config.hrl").

%% API.
start(_Type, _Args) ->
  Dispatch = cowboy_router:compile([
    {'_', [{"/websocket/", ws_handler, []}]}
  ]),
  {ok, _} = cowboy:start_clear(http, 100, [{port, 8888}], #{
    env => #{dispatch => Dispatch}
  }),
  websocket_sup:start_link().

stop(_State) ->
  ok.

