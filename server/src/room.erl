-module(room).
-behavior(gen_server).
-include("config.hrl").
-include("room_state.hrl").

-export([
  start_link/1, init/1, handle_info/2, terminate/1, find_player_pid/2,
  get_player_pid/2, handle_call/3, create_state_snapshot/1, set_state/2,
  terminate/2, handle_cast/2, code_change/3
]).

start_link(Room_id) ->
  gen_server:start_link(?MODULE, Room_id, []).

init(_Room_id) ->
  {ok, #room_state{id = _Room_id, life = ?EARTH_LIFE}}.

% synchronous messages
handle_call(_Request, _From, State) ->
  case _Request of
    {set_state_player, Player_id} ->
      Player_pid = player:start(whereis(slave_handler), Player_id),
      NewState = State#room_state{players = [{Player_id, Player_pid} | State#room_state.players]},
      {reply, ok, NewState};
    create_state_snapshot ->
      utils:log("Master local state room snapshot: ~n~p~n", [State]),
      {reply, State, State};
    {find_player_pid, Player_id} ->
      Player_pid = find_player_pid(State#room_state.players, Player_id),
      case Player_pid of
        error -> {reply, error, State};
        _ -> {reply, Player_pid, State}
      end
  end.

handle_info(Data, State) ->
  case Data of
    {broadcast_players, Msg} ->
      lists:flatmap(fun({_, Player_pid}) -> Player_pid ! Msg, [] end, State#room_state.players),
      {noreply, State};
    {room_id, Room_id} ->
      {noreply, State#room_state{id = Room_id}};
    {player_remove, Player_id} ->
      New_Ship_Positions = remove_ship_from_list(State#room_state.ships_position, get_ship_id(State#room_state.players, Player_id)),
      New_State = State#room_state{players = player_remove(State#room_state.players, Player_id), ships_position = New_Ship_Positions},
      if
        length(New_State#room_state.players) == 0 ->
          global_rooms_state ! {room_remove, State#room_state.id};
        true ->
          broadcast(New_State#room_state.players, room_players_number, length(New_State#room_state.players))
      end,
      {noreply, New_State};
    {player_add, {Player_id, Player_pid, Ship_id}} ->
      New_State = State#room_state{players = [{Player_id, Player_pid, Ship_id} | State#room_state.players]},
      Player_pid ! {game_life, New_State#room_state.life},
      broadcast(New_State#room_state.players, room_players_number, length(New_State#room_state.players)),
      if
        length(New_State#room_state.players) > 1 ->
          broadcast([lists:last(State#room_state.players)], asteroid_position, []);
        true -> ok
      end,
      {noreply, New_State};
    {action_earth_collision, PlayerId} ->
      [{Pid, _, _} | _] = State#room_state.players,
      if
        (Pid == PlayerId) -> New_life = State#room_state.life - ?EARTH_LIFE_DECREASE;
        true -> New_life = State#room_state.life
      end,
      if
        New_life >= 0 -> New_life;
        New_life -> New_life = 0
      end,
      New_State = State#room_state{life = New_life},
      broadcast(State#room_state.players, game_life, New_life),
      {noreply, New_State};
    {game_master_asteroids_position, Position} ->
      New_State = State#room_state{asteroids_position = Position},
      broadcast(State#room_state.players, asteroid_position_set, Position),
      {noreply, New_State};
    {ship_position, Position_ship} ->
      Position_list = [Position_ship | State#room_state.ships_position],
      New_State = State#room_state{ships_position = Position_list},
      broadcast(State#room_state.players, ship_position_set, Position_list),
      {noreply, New_State};
    {ship_move, [Ship_id, Direction]} ->
      New_State = State#room_state{ships_position = update_position(State#room_state.ships_position, Ship_id, Direction)},
      broadcast(State#room_state.players, ship_position_set, New_State#room_state.ships_position),
      {noreply, New_State};
    {ship_shoot, Ship_id} ->
      broadcast(State#room_state.players, ship_shoot, Ship_id),
      {noreply, State};
    stop ->
      terminate(self());
    Unknown ->
      utils:log("Warning: unknown message received in 'room', message: ~p~n", [Unknown]),
      {noreply, State}
  end.

update_position([[Ship_id, X, Y, Z] | XS], Ship_id, Direction) ->
  case binary_to_list(Direction) of
    "x+" -> [[Ship_id, X + ?SHIP_MOVE_FACTOR_X, Y, Z] | XS];
    "x-" -> [[Ship_id, X - ?SHIP_MOVE_FACTOR_X, Y, Z] | XS];
    "y+" -> [[Ship_id, X, Y + ?SHIP_MOVE_FACTOR_Y, Z] | XS];
    "y-" -> [[Ship_id, X, Y - ?SHIP_MOVE_FACTOR_Y, Z] | XS];
    "z+" -> [[Ship_id, X, Y, Z + ?SHIP_MOVE_FACTOR_Z] | XS];
    "z-" -> [[Ship_id, X, Y, Z - ?SHIP_MOVE_FACTOR_Z] | XS]
  end;
update_position([X | XS], Ship_id, Direction) -> lists:append([X], update_position(XS, Ship_id, Direction));
update_position([], _, _) -> [].

get_ship_id([{Player_id, Player_pid, Ship_id} | XS] , Player_id) ->
  Ship_id;
get_ship_id([X | XS], Player_id) -> get_ship_id (XS, Player_id);
get_ship_id([], Player_id) -> error.


player_remove([{Player_id, Player_pid, Ship_id} | XS], Player_id) ->
  broadcast(XS, remove_ship_scene, Ship_id),
  Player_pid ! stop,
  XS;
player_remove([X | XS], Player_id) -> lists:append([X], player_remove(XS, Player_id));
player_remove([], _) -> [].

remove_ship_from_list([[Ship_id, _, _, _] | XS], Ship_id) ->
  XS;
remove_ship_from_list([X | XS], Ship_id) -> lists:append([X], remove_ship_from_list(XS, Ship_id));
remove_ship_from_list([], _) -> [].

find_player_pid([{Player_id, Player_pid} | _], Player_id) ->
  Player_pid;
find_player_pid([{_, _} | Players], Player_id) ->
  find_player_pid(Players, Player_id);
find_player_pid([], _) ->
  error.

get_player_pid(Room_pid, Player_id) ->
  gen_server:call(Room_pid, {find_player_pid, Player_id}).

broadcast([], _, _) -> ok;
broadcast([{_, Player_pid, _} | XS], Event, Data) ->
  Player_pid ! {Event, Data},
  broadcast(XS, Event, Data).

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

handle_cast(_Request, State) ->
  case _Request of
    {set_state, NewState} ->
      utils:log("Updating room state to: ~n~p~n", [State]),
      {noreply, NewState}
  end.

terminate(_Reason, _State) ->
  ok.

terminate(PID) ->
  utils:log("Killed: room, pid: ~p~n", [PID]),
  exit(PID, kill).

set_state(Room_pid, State) ->
  gen_server:cast(Room_pid, {set_state, State}).


create_state_snapshot(Room_pid) ->
  utils:log("Creating local master snapshot of room with pid: ~p~n", [Room_pid]),
  gen_server:call(Room_pid, create_state_snapshot).
