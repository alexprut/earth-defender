-module(ws_handler).

-export([init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-import(counter, [inc/0, dec/0, current/0]).


init(Req, Opts) -> {cowboy_websocket, Req, Opts, 120000}.

websocket_handle({text, Msg}, Req, State) ->
  case binary_to_list(Msg) of
    "add_player" ->
      inc(),
      {ok, Req, State};
    "rem_player" ->
      dec(),
      {ok, Req, State};
    "ret_player" ->
      {reply, {text, list_to_binary(current())}, Req, State}
  end;
websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.


websocket_info(_Info, Req, State) ->
  {ok, Req, State}.
