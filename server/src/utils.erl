-module(utils).

-include("config.hrl").

%% External exports
-export([generate_uuid/0, get_initial_role/0, get_master_name/0, get_port/0, get_service_url/0, log/2]).

%% Internal exports
-export([]).

%%% ---------------------------------------------------
%%%
%%% Utilities functions.
%%%
%%% ---------------------------------------------------

generate_uuid() ->
  erlang:list_to_bitstring(
    erlang:ref_to_list(
      erlang:make_ref()
    )
  ).

log(Msg, Args) ->
  case ?DEBUG of
    true ->
      io:format(user, Msg, Args);
    _ ->
      ok
  end.

get_initial_role() ->
  case init:get_argument(role) of
    error ->
      master;
    _ ->
      slave
  end.

get_master_name() ->
  case init:get_argument(master_name) of
    error ->
      error;
    {_, [[Master_name]]} ->
      erlang:list_to_atom(Master_name)
  end.

get_port() ->
  case init:get_argument(port) of
    error ->
      Websocket_port = ?WEBSOCKET_PORT;
    {_, [[Port]]} ->
      {Websocket_port, []} = string:to_integer(Port)
  end,
  Websocket_port.

get_service_url() ->
  case init:get_argument(hostname) of
    error ->
      Hostname = ?DEFAULT_HOSTNAME;
    {_, Hostname} ->
      ok
  end,
  list_to_bitstring(Hostname ++ ":" ++ integer_to_list(get_port()) ++ "/websocket").

