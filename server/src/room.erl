-module(room).

-export([start/1, loop/1]).

% Data in #state.players saved as: {player_id, player_pid}
-record(state, {players = [], id, life = 1000, a_position}).

start(RoomId) -> spawn(room, loop, [#state{id = RoomId}]).

loop(State) ->
  receive
    {room_id, RoomId} ->
      loop(State#state{id = RoomId});
    {player_remove, PlayerId} ->
      NewState = State#state{players = player_remove(State#state.players, PlayerId)},
      if
        length(NewState#state.players) == 0 ->
          global_rooms_state ! {room_remove, State#state.id};
        true ->
          broadcast(NewState#state.players, room_players_number, length(NewState#state.players))
      end,
      loop(NewState);
    {player_add, {PlayerId, PlayerPID}} ->
      NewState = State#state{players = [{PlayerId, PlayerPID} | State#state.players]},
      PlayerPID ! {game_life, NewState#state.life},
      broadcast(NewState#state.players, room_players_number, length(NewState#state.players)),
      loop(NewState);
    {action_earth_collision} ->
      NewLife = State#state.life - 200,
      if
        NewLife >= 0 -> NewLife = NewLife;
        NewLife -> NewLife = 0
      end,
      NewState = State#state{life = NewLife},
      broadcast(State#state.players, game_life, NewLife),
      loop(NewState);
    {action_new_player_join} ->
      broadcast([lists:last(State#state.players)], asteroid_position, []),
      loop(State);
    {master_asteroid_position, Position} ->
      NewState = State#state{a_position = Position},
      broadcast(State#state.players, asteroid_position_set, Position),
      loop(NewState)
  end.

player_remove([{PlayerId, PlayerPID} | XS], PlayerId) ->
  PlayerPID ! stop,
  XS;
player_remove([X | XS], PlayerId) -> lists:append([X], player_remove(XS, PlayerId));
player_remove([], _) -> [].

broadcast([], _, _) -> ok;
broadcast([{_, Player_pid} | XS], Event, Data) ->
  Player_pid ! {Event, Data},
  broadcast(XS, Event, Data).

stop() -> exit(self(), normal).
