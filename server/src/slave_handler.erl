-module(slave_handler).
-behavior(gen_server).

-export([handle_info/2, start_link/1, init/1, handle_call/3, handle_cast/2, code_change/3, terminate/2, connect_to_master/1]).

-record(state, {master_name, player_id = undef, room_id = undef, room_pid = undef, player_pid = undef}).

start_link(Master_name) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Master_name], []).

init(_Args) ->
  {ok, #state{master_name = _Args}}.

handle_info(Data, State) ->
  case Data of
    Unknown ->
      io:format("Warning: unknown message received in 'slave_handler', message: ~p~n", [Unknown]),
      {noreply, State}
  end.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% synchronous messages
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

% asynchronous messages
handle_cast({Event, Data}, State) ->
  io:format("Receiving message:~n~p~n", [{Event, Data}]),
  case Event of
    "room_join" ->
      {Room_id, Player_id} = Data,
      Room_pid = global_rooms_state:get_room_pid(Room_id),
      Player_pid = player:start(self(), Player_id),
      Room_pid ! {player_add, {Player_id, Player_pid}},
      New_state = State#state{player_id = Player_id, room_id = Room_id, player_pid = Player_pid, room_pid = Room_pid},
      {noreply, New_state};
    "room_add" ->
      {Room_id, Player_id} = Data,
      {_, Room_pid} = room:start_link(Room_id),
      io:format("Room id:~n~p~n", [Room_id]),
      global_rooms_state:add_room(Room_id, Room_pid),
      Player_pid = player:start(self(), Player_id),
      Room_pid ! {player_add, {Player_id, Player_pid}},
      New_state = State#state{player_id = Player_id, room_id = Room_id, player_pid = Player_pid, room_pid = Room_pid},
      {noreply, New_state};
    "action_earth_collision" ->
      State#state.room_pid ! {action_earth_collision, State#state.player_id},
      {noreply, State};
    "game_master_asteroids_position" ->
      State#state.room_pid ! {game_master_asteroids_position, Data},
      {noreply, State};
    "game_ship_position" ->
      State#state.room_pid ! {ship_position, Data},
      {noreply, State};
    "action_ship_move" ->
      State#state.room_pid ! {ship_move, Data},
      {noreply, State};
    "action_ship_shoot" ->
      State#state.room_pid ! {ship_shoot, Data},
      {noreply, State};
    Unknown ->
      io:format("Warning: 'slave_handler' can not handle event:~n~p~n", [Unknown]),
      {noreply, State}
  end;
handle_cast(_Request, State) ->
  case _Request of
    Unknown ->
      io:format("Warning: unknown message received in 'global_room_state:handle_cast', message: ~p~n", [Unknown]),
      {noreply, State}
  end.

terminate(_Reason, _State) ->
  ok.

connect_to_master(Master_name) ->
  net_kernel:connect_node(Master_name),
  {global_rooms_state, Master_name} ! {'slave_connect', node(), utils:get_service_url()}.
