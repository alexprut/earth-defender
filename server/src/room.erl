-module(room).
-include("config.hrl").

-export([start/1, loop/1, terminate/1]).

% Data in #state.players saved as: {player_id, player_pid}
-record(state, {players = [], id, life = ?EARTH_LIFE, asteroids_position, ships_position = [[]]}).

start(Room_id) -> spawn(room, loop, [#state{id = Room_id}]).

loop(State) ->
  receive
    {room_id, Room_id} ->
      loop(State#state{id = Room_id});
    {player_remove, Player_id} ->
      New_State = State#state{players = player_remove(State#state.players, Player_id)},
      if
        length(New_State#state.players) == 0 ->
          global_rooms_state ! {room_remove, State#state.id};
        true ->
          broadcast(New_State#state.players, room_players_number, length(New_State#state.players))
      end,
      loop(New_State);
    {player_add, {Player_id, Player_pid}} ->
      New_State = State#state{players = [{Player_id, Player_pid} | State#state.players]},
      Player_pid ! {game_life, New_State#state.life},
      broadcast(New_State#state.players, room_players_number, length(New_State#state.players)),
      loop(New_State);
    {action_earth_collision, Pid} ->
%      X = lists:last(State#state.players),
%      [{Pid,_}|_] = X,
      New_life = State#state.life - 200,
%      if
%        Pid == X -> New_life = State#state.life - 200;
%        New_life -> New_life = State#state.life
%      end,
      if
        New_life >= 0 -> New_life = New_life;
        New_life -> New_life = 0
      end,
      New_State = State#state{life = New_life},
      broadcast(State#state.players, game_life, New_life),
      loop(New_State);
    {action_new_player_join} ->
      broadcast([lists:last(State#state.players)], asteroid_position, []),
      loop(State);
    {master_asteroid_position, Position} ->
      New_State = State#state{asteroids_position = Position},
      broadcast(State#state.players, asteroid_position_set, Position),
      loop(New_State);
    {ship_position, Position_ship} ->
      Position_list = [Position_ship | State#state.ships_position],
      New_State = State#state{ships_position = Position_list},
      broadcast(State#state.players, ship_position_set, Position_list),
      loop(New_State);
    {ship_move, [Ship_id, Direction]} ->
      New_State = State#state{ships_position = update_position(State#state.ships_position, Ship_id, Direction)},
      broadcast(State#state.players, ship_position_set, State#state.ships_position),
      loop(New_State);
    {ship_shoot, Ship_id} ->
      broadcast(State#state.players, ship_shoot, Ship_id),
      loop(State)
  end.

update_position([[Ship_id, X, Y, Z] | XS], Ship_id, Direction) ->
  case binary_to_list(Direction) of
    "x+" -> [[Ship_id, X + 2, Y, Z] | XS];
    "x-" -> [[Ship_id, X - 2, Y, Z] | XS];
    "y+" -> [[Ship_id, X, Y + 2, Z] | XS];
    "y-" -> [[Ship_id, X, Y - 2, Z] | XS];
    "z+" -> [[Ship_id, X, Y, Z + 5] | XS];
    "z-" -> [[Ship_id, X, Y, Z - 5] | XS]
  end;
update_position([X | XS], Ship_id, Direction) -> lists:append([X], update_position(XS, Ship_id, Direction));
update_position([], _, _) -> [].

player_remove([{Player_id, Player_pid} | XS], Player_id) ->
  Player_pid ! stop,
  XS;
player_remove([X | XS], Player_id) -> lists:append([X], player_remove(XS, Player_id));
player_remove([], _) -> [].

broadcast([], _, _) -> ok;
broadcast([{_, Player_pid} | XS], Event, Data) ->
  Player_pid ! {Event, Data},
  broadcast(XS, Event, Data).

terminate(PID) -> exit(PID, kill).
