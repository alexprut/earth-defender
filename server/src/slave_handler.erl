-module(slave_handler).

-behavior(gen_server).

-include("room_state.hrl").

%% External exports
-export([start_link/1, connect_to_master/1, get_master_data/0]).

%% Internal exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {master_pid, master_name, master_service_url}).

%%% ---------------------------------------------------
%%%
%%% gen_server.
%%%
%%% ---------------------------------------------------

start_link(Arg) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Arg, []).

init({Master_name, Master_pid, Master_service_url}) ->
  {ok, #state{master_pid = Master_pid, master_name = Master_name, master_service_url = Master_service_url}}.

% Synchronous messages
handle_call(_Request, _From, State) ->
  case _Request of
    get_master_data ->
      Master = {
        State#state.master_name,
        State#state.master_pid,
        State#state.master_service_url
      },
      {reply, Master, State};
    Unknown ->
      utils:log("Warning: unknown message received in 'slave_handler:handle_call', message: ~p~n", [Unknown]),
      {noreply, State}
  end.

% Asynchronous messages
handle_cast({Event, Data}, State) ->
  utils:log("Receiving message in Slave:~n~p~n", [{Event, Data}]),
  case Event of
    set_state_master ->
      {Master_name, Master_pid, Master_service_url} = Data,
      New_state = State#state{
        master_pid = Master_pid,
        master_name = Master_name,
        master_service_url = Master_service_url
      },
      monitors:start_monitor_master(New_state#state.master_pid),
      {noreply, New_state};
    set_state_slaves ->
      set_state([], Data),
      utils:log("Updated slaves list.~n", []),
      {noreply, State};
    "room_join" ->
      {Room_id, Player_id, Ship_id} = Data,
      Room_pid = local_rooms_state:get_room_pid(Room_id),
      Player_pid = player:start(self(), Player_id, Ship_id),
      Room_pid ! {player_add, {Player_id, Player_pid, Ship_id}},
      {noreply, State};
    "room_add" ->
      {Room_id, Player_id, Ship_id} = Data,
      {_, Room_pid} = room:start_link(Room_id),
      utils:log("Room id:~n~p~n", [Room_id]),
      local_rooms_state:add_room(Room_id, Room_pid),
      Player_pid = player:start(self(), Player_id, Ship_id),
      Room_pid ! {player_add, {Player_id, Player_pid, Ship_id}},
      New_state = State#state{},
      {noreply, New_state};
    "action_earth_collision" ->
      {Room_id, Player_id} = Data,
      Room_pid = local_rooms_state:find_room_pid(Room_id),
      Room_pid ! {action_earth_collision, Player_id},
      {noreply, State};
    "game_master_asteroids_position" ->
      {Room_id, Msg} = Data,
      Room_pid = local_rooms_state:find_room_pid(Room_id),
      Room_pid ! {game_master_asteroids_position, Msg},
      {noreply, State};
    "game_ship_position" ->
      {Room_id, Msg} = Data,
      Room_pid = local_rooms_state:find_room_pid(Room_id),
      Room_pid ! {ship_position, Msg},
      {noreply, State};
    "action_ship_move" ->
      {Room_id, Msg} = Data,
      Room_pid = local_rooms_state:find_room_pid(Room_id),
      Room_pid ! {ship_move, Msg},
      {noreply, State};
    "action_ship_shoot" ->
      {Room_id, Msg} = Data,
      Room_pid = local_rooms_state:find_room_pid(Room_id),
      Room_pid ! {ship_shoot, Msg},
      {noreply, State};
    Unknown ->
      utils:log("Warning: 'slave_handler:handle_cast' can not handle event:~n~p~n", [Unknown]),
      {noreply, State}
  end;
handle_cast(_Request, State) ->
  case _Request of
    Unknown ->
      utils:log("Warning: unknown message received in 'slave_handler:handle_cast', message: ~p~n", [Unknown]),
      {noreply, State}
  end.

% Asynchronous messages
handle_info(Data, State) ->
  case Data of
    {set_state, {Rooms, Slaves}} ->
      set_state(Rooms, Slaves),
      utils:log("Slave finished to copy & set master state.~n", []),
      monitors:start_monitor_master(State#state.master_pid),
      {noreply, State};
    Unknown ->
      utils:log("Warning: unknown message received in 'slave_handler:handle_info', message: ~p~n", [Unknown]),
      {noreply, State}
  end.

terminate(Reason, _State) ->
  utils:log("Terminating 'slave_handler', reason: ~p~n", [Reason]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%% ---------------------------------------------------
%%%
%%% gen_server calls: utilities functions.
%%%
%%% ---------------------------------------------------

get_master_data() ->
  gen_server:call(whereis(slave_handler), {get_master_data}).

set_state(Rooms, Slaves) ->
  add_rooms(Rooms),
  local_rooms_state:set_state_slaves(Slaves).

add_rooms([State | Rooms]) ->
  add_rooms(Rooms),
  {_, Room_pid} = room:start_link(State#room_state.id),
  utils:log("Room pid in slave: ~p~n", [Room_pid]),
  room:set_state(Room_pid, State),
  local_rooms_state:add_room(State#room_state.id, Room_pid);
add_rooms([]) ->
  ok.

%%% ---------------------------------------------------
%%%
%%% Utilities functions.
%%%
%%% ---------------------------------------------------

connect_to_master(Master_name) ->
  utils:log("Request from slave to connect to master: ~n~p~n", [Master_name]),
  case net_kernel:connect_node(Master_name) of
    true ->
      {local_rooms_state, Master_name} ! {'slave_connect', node(), utils:get_service_url()};
    _ ->
      utils:log("Can't connect to the master, kill me: ~n~p~n", [Master_name]),
      application:stop(earth_defender_app)
  end.
