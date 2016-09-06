-module(slave_handler).
-behavior(gen_server).
-include("room_state.hrl").

-export([
  handle_info/2, start_link/2, init/1, handle_call/3, handle_cast/2, code_change/3, terminate/2,
  connect_to_master/1, set_state/2
]).

-record(state, {master_name, master_pid}).

start_link(Master_name, Master_pid) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Master_name, Master_pid], []).

init([Master_name, Master_pid]) ->
  {ok, #state{master_name = Master_name, master_pid = Master_pid}}.

handle_info(Data, State) ->
  case Data of
    {set_state, {Rooms, Slaves}} ->
      set_state(Rooms, Slaves),
      utils:log("Slave finished to copy & set master state.~n", []),
      monitors:start_monitor_master(State#state.master_pid),
      {noreply, State};
    Unknown ->
      utils:log("Warning: unknown message received in 'slave_handler', message: ~p~n", [Unknown]),
      {noreply, State}
  end.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% synchronous messages
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

% asynchronous messages
handle_cast({Event, Data}, State) ->
  utils:log("Receiving message in Slave:~n~p~n", [{Event, Data}]),
  case Event of
    "room_join" ->
      {Room_id, Player_id} = Data,
      Room_pid = global_rooms_state:get_room_pid(Room_id),
      Player_pid = player:start(self(), Player_id),
      Room_pid ! {player_add, {Player_id, Player_pid}},
      {noreply, State};
    "room_add" ->
      {Room_id, Player_id} = Data,
      {_, Room_pid} = room:start_link(Room_id),
      utils:log("Room id:~n~p~n", [Room_id]),
      global_rooms_state:add_room(Room_id, Room_pid),
      Player_pid = player:start(self(), Player_id),
      Room_pid ! {player_add, {Player_id, Player_pid}},
      New_state = State#state{},
      {noreply, New_state};
    "action_earth_collision" ->
      {Room_id, Player_id, Msg} = Data,
      Room_pid = global_rooms_state:find_room_pid(Room_id),
      Room_pid ! {action_earth_collision, Player_id},
      {noreply, State};
    "game_master_asteroids_position" ->
      {Room_id, Msg} = Data,
      Room_pid = global_rooms_state:find_room_pid(Room_id),
      Room_pid ! {game_master_asteroids_position, Msg},
      {noreply, State};
    "game_ship_position" ->
      {Room_id, Msg} = Data,
      Room_pid = global_rooms_state:find_room_pid(Room_id),
      Room_pid ! {ship_position, Msg},
      {noreply, State};
    "action_ship_move" ->
      {Room_id, Msg} = Data,
      Room_pid = global_rooms_state:find_room_pid(Room_id),
      Room_pid ! {ship_move, Msg},
      {noreply, State};
    "action_ship_shoot" ->
      {Room_id, Msg} = Data,
      Room_pid = global_rooms_state:find_room_pid(Room_id),
      Room_pid ! {ship_shoot, Msg},
      {noreply, State};
    Unknown ->
      utils:log("Warning: 'slave_handler' can not handle event:~n~p~n", [Unknown]),
      {noreply, State}
  end;
handle_cast(_Request, State) ->
  case _Request of
    Unknown ->
      utils:log("Warning: unknown message received in 'slave_handler:handle_cast', message: ~p~n", [Unknown]),
      {noreply, State}
  end.

terminate(_Reason, _State) ->
  ok.

set_state(Rooms, Slaves) ->
  add_rooms(Rooms).

add_rooms([State | Rooms]) ->
  {_, Room_pid} = room:start_link(State#room_state.id),
  utils:log("Room pid in slave: ~p~n", [Room_pid]),
  room:set_state(Room_pid, State),
  global_rooms_state:add_room(State#room_state.id, Room_pid),
  add_rooms(Rooms);
add_rooms([]) ->
  ok.

connect_to_master(Master_name) ->
  utils:log("Request from slave to connect to master: ~n~p~n", [Master_name]),
  net_kernel:connect_node(Master_name),
  {global_rooms_state, Master_name} ! {'slave_connect', node(), utils:get_service_url()}.
