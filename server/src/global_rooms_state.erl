-module(global_rooms_state).
-behavior(gen_server).

-export([
  handle_info/2, start_link/0, init/1, handle_call/3, handle_cast/2, code_change/3, terminate/2,
  get_room_pid/1, get_rooms_list/0, add_room/2, broadcast_slaves/2, init_broadcast_slaves/1, get_servers_list/0,
  is_master/0, set_state_slaves/1, search_room_pid/2, find_room_pid/1, slave_remove/1, broadcast_players/2
]).

% Data in #state.rooms saved as: {room_id, room_pid}
% #state.slaves saved ad: {slave_name, slave_pid, slave_service_url}
-record(state, {rooms = [], slaves = [], role}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
  Role = utils:get_initial_role(),
  case Role of
    master ->
      ok;
    slave ->
      slave_handler:connect_to_master(utils:get_master_name())
  end,
  {ok, #state{role = Role}}.

handle_info(Data, State) ->
  case Data of
    {set_state_slaves, State_slaves} ->
      New_state = State#state{slaves = State_slaves},
      {noreply, New_state};
    {slave_remove, Slave_pid} ->
      New_state = State#state{slaves = slave_remove(State#state.slaves, Slave_pid)},
      utils:log("Removed slave of pid. ~p~n", [Slave_pid]),
      broadcast_slaves(New_state#state.slaves, {set_state_slaves, New_state#state.slaves}),
      broadcast_players(New_state#state.rooms, {broadcast_players, {servers_list, create_servers_list(New_state)}}),
      {noreply, New_state};
    master_takeover ->
      utils:log("Slave attempting to be the new master: ~n", []),
      [Slave_master | Slaves] = State#state.slaves,
      Pid = whereis(slave_handler),
      case Slave_master of
        {Name, Pid, Service_url} ->
          utils:log("Slave becoming the new master, name: ~p~n", [Name]),
          New_state = State#state{role = master, slaves = Slaves},
          % Broadcast slaves the new slaves
          broadcast_slaves(New_state#state.slaves, {set_state_slaves, Slaves}),
          % Broadcast to slaves the new master data
          broadcast_slaves(New_state#state.slaves, {set_state_master, {Name, Pid, Service_url}}),
          % Monitor Slaves
          lists:flatmap(fun({_, Slave_pid, _}) -> [monitors:start_monitor_slave(Slave_pid)] end, Slaves),
          broadcast_players(New_state#state.rooms, {broadcast_players, {servers_list, create_servers_list(New_state)}}),
          {noreply, New_state};
        _ ->
          {noreply, State}
      end;
    {room_remove, Room_id} ->
      New_state = State#state{rooms = room_remove(State#state.rooms, Room_id)},
      {noreply, New_state};
    {slave_connect, Slave_name, Service_url} ->
      utils:log("Master connects to slave: ~p~n", [Slave_name]),
      case rpc:call(Slave_name, slave_handler, start_link, [{node(), self(), utils:get_service_url()}], 2000) of
        {_, Slave_pid} ->
          % Add slave to state
          New_state = State#state{slaves = [{Slave_name, Slave_pid, list_to_bitstring(Service_url)} | State#state.slaves]},
          % Creating local master snapshot
          utils:log("Starting creating local master snapshot...~n", []),
          Snapshot = {
            lists:flatmap(fun({_, Room_pid}) -> [room:create_state_snapshot(Room_pid)] end, State#state.rooms),
            State#state.slaves
          },
          utils:log("Master local state snapshot: ~n~p~n", [Snapshot]),
          % Send state snapshot to slave
          utils:log("Master sends local state snapshot to slave: ~p~n", [Slave_name]),
          Slave_pid ! {set_state, Snapshot},
          % Start monitoring the slave
          monitors:start_monitor_slave(Slave_pid),
          broadcast_slaves(New_state#state.slaves, {set_state_slaves, New_state#state.slaves}),
          broadcast_players(New_state#state.rooms, {broadcast_players, {servers_list, create_servers_list(New_state)}});
        _ ->
          New_state = State
      end,
      {noreply, New_state};
    Unknown ->
      utils:log("Warning: unknown message received in 'global_room_state', message: ~p~n", [Unknown]),
      {noreply, State}
  end.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% synchronous messages
handle_call(_Request, _From, State) ->
  case _Request of
    {find_room_pid, Room_id} ->
      {reply, search_room_pid(Room_id, State#state.rooms), State};
    is_master ->
      case State#state.role of
        master ->
          {reply, true, State};
        _ ->
          {reply, false, State}
      end;
    {room_add, {Room_id, Room_pid}} ->
      New_state = State#state{rooms = [{Room_id, Room_pid} | State#state.rooms]},
      utils:log("Rooms: ~p~n", [New_state#state.rooms]),
      {reply, ok, New_state};
    get_rooms_list ->
      Rooms_list = lists:flatmap(fun(Room) -> {Id, _} = Room, [Id] end, State#state.rooms),
      utils:log("Rooms list: ~p~n", [Rooms_list]),
      {reply, Rooms_list, State};
    {get_room_pid, Room_id} ->
      {reply, search_room_pid(Room_id, State#state.rooms), State};
    servers_list ->
      Server_list = create_servers_list(State),
      {reply, Server_list, State};
    Unknown ->
      utils:log("Warning: unknown message received in 'global_room_state:handle_cast', message: ~p~n", [Unknown]),
      {reply, ok, State}
  end.

% asynchronous messages
handle_cast({Event, Data}, State) ->
  utils:log("Broadcast to Slaves: ~p~n", [State#state.slaves]),
  broadcast_slaves(State#state.slaves, {Event, Data}),
  {noreply, State};
handle_cast(_Request, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

add_room(Room_id, Room_pid) ->
  gen_server:call(whereis(global_rooms_state), {room_add, {Room_id, Room_pid}}).

get_room_pid(Room_id) ->
  gen_server:call(whereis(global_rooms_state), {get_room_pid, Room_id}).

get_rooms_list() ->
  gen_server:call(whereis(global_rooms_state), get_rooms_list).

search_room_pid(Room_id, [{Room_id, Room_pid} | _]) -> Room_pid;
search_room_pid(Room_id, [_ | XS]) -> search_room_pid(Room_id, XS);
search_room_pid(Room_id, []) ->
  utils:log("Warning: there is no such a room of id: ~p~n", [Room_id]),
  [].

slave_remove([{_, Slave_pid, _} | XS], Slave_pid) ->
  XS;
slave_remove([X | XS], S) ->
  lists:append([X], slave_remove(XS, S));
slave_remove([], _) ->
  utils:log("Warning: there is no such a slave to be removed. ~n", []),
  [].

room_remove([{Room_id, Room_pid} | XS], Room_id) ->
  Room_pid ! stop,
  XS;
room_remove([X | XS], R) -> lists:append([X], room_remove(XS, R));
room_remove([], _) -> [].

broadcast_slaves([{_, Slave_pid, _} | Slaves], Data) ->
  gen_server:cast(Slave_pid, Data),
  broadcast_slaves(Slaves, Data);
broadcast_slaves([], _) ->
  ok.

create_servers_list(State) ->
  case State#state.role of
    master ->
      Is_master = true;
    _ ->
      Is_master = false
  end,
  case Is_master of
    true ->
      Master_service = list_to_bitstring(utils:get_service_url());
    false ->
      {_, _, Service_url} = slave_handler:get_master_data(),
      Master_service = Service_url
  end,
  Server_list = lists:append([
    [Master_service],
    lists:flatmap(fun({_, _, Service_url}) -> [Service_url] end, State#state.slaves)
  ]),
  Server_list.

broadcast_players(Rooms, Msg) ->
  lists:flatmap(fun({_, Room_pid}) -> Room_pid ! Msg, [] end, Rooms).

find_room_pid(Room_id) ->
  gen_server:call(whereis(global_rooms_state), {find_room_pid, Room_id}).

set_state_slaves(State_slaves) ->
  whereis(global_rooms_state) ! {set_state_slaves, State_slaves}.

init_broadcast_slaves(Data) ->
  gen_server:cast(whereis(global_rooms_state), Data).

get_servers_list() ->
  gen_server:call(whereis(global_rooms_state), servers_list).

is_master() ->
  gen_server:call(whereis(global_rooms_state), is_master).

slave_remove(Slave_pid) ->
  whereis(global_rooms_state) ! {slave_remove, Slave_pid}.
