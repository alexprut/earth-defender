-module(room).

-export([start/1, loop/1]).

% Data in #state.players saved as: {player_id, player_pid}
-record(state, {players = [], id}).

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
          broadcast_players_number(NewState#state.players, length(NewState#state.players))
      end,
      loop(NewState);
    {player_add, {PlayerId, PlayerPID}} ->
      NewState = State#state{players = [{PlayerId, PlayerPID} | State#state.players]},
      broadcast_players_number(NewState#state.players, length(NewState#state.players)),
      loop(NewState)
  end.

player_remove([{PlayerId, PlayerPID} | XS], PlayerId) ->
  PlayerPID ! stop,
  XS;
player_remove([X | XS], PlayerId) -> lists:append([X], player_remove(XS, PlayerId));
player_remove([], _) -> [].

broadcast_players_number([], _) -> ok;
broadcast_players_number([{PlayerId, PlayerPID} | XS], Players_number) ->
  PlayerPID ! {room_players_number, Players_number},
  broadcast_players_number(XS, Players_number).

stop() -> exit(self(), normal).
