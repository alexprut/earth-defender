-module(slave_handler).
-behavior(gen_server).

-export([handle_info/2, start_link/1, init/1, handle_call/3, handle_cast/2, code_change/3, terminate/2, connect_to_master/1]).

% Data in #state.rooms saved as: {room_id, room_pid}
-record(state, {rooms = [], master_name}).

start_link(Master_name) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Master_name], []).

init(_Args) ->
  {ok, #state{master_name = _Args}}.

handle_info(Info, State) ->
  case Info of
    Unknown ->
      io:format("Warning: unknown message received in 'global_room_state', message: ~p~n", [Unknown]),
      {noreply, State}
  end.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% synchronous messages
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

% asynchronous messages
handle_cast(_Request, State) ->
  case _Request of
    Unknown ->
      io:format("Warning: unknown message received in 'global_room_state:handle_cast', message: ~p~n", [Unknown]),
      {reply, ok, State}
  end.

terminate(_Reason, _State) ->
  ok.

connect_to_master(Master_name) ->
  net_kernel:connect_node(Master_name),
  {global_rooms_state, Master_name} ! {'slave_connected', node()}.
