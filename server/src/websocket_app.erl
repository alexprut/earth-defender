%% @private
-module(websocket_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).
-import(counter, [start/0, inc/0]).


%% API.
start(_Type, _Args) ->
  counter:start(),
  inc(),
  Dispatch = cowboy_router:compile([
    {'_', [{"/websocket/", ws_handler, []}]}
  ]),
  {ok, _} = cowboy:start_clear(http, 100, [{port, 8888}], #{
    env => #{dispatch => Dispatch}
  }),
  websocket_sup:start_link().

stop(_State) ->
  ok.
