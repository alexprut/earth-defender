-module(room).
-behavior('gen_server').
-include("config.hrl").

-export([start_link/1, init/1, handle_info/2, terminate/1, find_player_pid/2, get_player_pid/2, handle_call/3]).

% Data in #state.players saved as: {player_id, player_pid}
-record(state, {players = [], id, life = ?EARTH_LIFE, asteroids_position, ships_position = [[]]}).

start_link(Room_id) ->
  gen_server:start_link(?MODULE, [Room_id], []).

init(_Room_id) ->
  {ok, #state{id = _Room_id}}.

% synchronous messages
handle_call(_Request, _From, State) ->
  erlang:display("asdasd"),
  case _Request of
    {find_player_pid, Player_id} ->
      Player_pid = find_player_pid(State#state.players, Player_id),
      case Player_pid of
        error -> {reply, error, State};
        _ -> {reply, Player_pid, State}
      end
  end.

handle_info(Data, State) ->
  case Data of
    {room_id, Room_id} ->
      {noreply, State#state{id = Room_id}};
    {player_remove, Player_id} ->
      New_State = State#state{players = player_remove(State#state.players, Player_id)},
      if
        length(New_State#state.players) == 0 ->
          global_rooms_state ! {room_remove, State#state.id};
        true ->
          broadcast(New_State#state.players, room_players_number, length(New_State#state.players))
      end,
      {noreply, New_State};
    {player_add, {Player_id, Player_pid}} ->
      New_State = State#state{players = [{Player_id, Player_pid} | State#state.players]},
      Player_pid ! {game_life, New_State#state.life},
      broadcast(New_State#state.players, room_players_number, length(New_State#state.players)),
      if
        length(New_State#state.players) > 1 ->
          broadcast([lists:last(State#state.players)], asteroid_position, []);
        true -> ok
      end,
      {noreply, New_State};
    {action_earth_collision, PlayerId} ->
      [{Pid, _} | _] = State#state.players,
      if
        (Pid == PlayerId) -> New_life = State#state.life - ?EARTH_LIFE_DECREASE;
        true -> New_life = State#state.life
      end,
      if
        New_life >= 0 -> New_life;
        New_life -> New_life = 0
      end,
      New_State = State#state{life = New_life},
      broadcast(State#state.players, game_life, New_life),
      {noreply, New_State};
    {game_master_asteroids_position, Position} ->
      New_State = State#state{asteroids_position = Position},
      broadcast(State#state.players, asteroid_position_set, Position),
      {noreply, New_State};
    {ship_position, Position_ship} ->
      Position_list = [Position_ship | State#state.ships_position],
      New_State = State#state{ships_position = Position_list},
      broadcast(State#state.players, ship_position_set, Position_list),
      {noreply, New_State};
    {ship_move, [Ship_id, Direction]} ->
      New_State = State#state{ships_position = update_position(State#state.ships_position, Ship_id, Direction)},
      broadcast(State#state.players, ship_position_set, New_State#state.ships_position),
      {noreply, New_State};
    {ship_shoot, Ship_id} ->
      broadcast(State#state.players, ship_shoot, Ship_id),
      {noreply, State};
    stop ->
      terminate(self());
    Unknown ->
      io:format("Warning: unknown message received in 'room', message: ~p~n", [Unknown]),
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

player_remove([{Player_id, Player_pid} | XS], Player_id) ->
  Player_pid ! stop,
  XS;
player_remove([X | XS], Player_id) -> lists:append([X], player_remove(XS, Player_id));
player_remove([], _) -> [].

find_player_pid([{Player_id, Player_pid} | _], Player_id) ->
  Player_pid;
find_player_pid([{_, _} | Players], Player_id) ->
  find_player_pid(Players, Player_id);
find_player_pid([], _) ->
  error.

get_player_pid(Room_pid, Player_id) ->
  gen_server:call(Room_pid, {find_player_pid, Player_id}).

broadcast([], _, _) -> ok;
broadcast([{_, Player_pid} | XS], Event, Data) ->
  Player_pid ! {Event, Data},
  broadcast(XS, Event, Data).

terminate(PID) ->
  io:format("Killed: room, pid: ~p~n", [PID]),
  exit(PID, kill).
