-module(room).

-export([start/1, loop/1]).

% Data in #state.players saved as: {player_id, player_pid}
-record(state, {players = [], id}).

start(RoomId) -> spawn(room, loop, [#state{id = RoomId}]).

loop(State) ->
  receive
    {room_id, RoomId} ->
      loop(State#state{id = RoomId});
    {player_remove, PlayerId, From} ->
      NewState = State#state{players = player_remove(State#state.players, PlayerId)},
      if
        length(NewState#state.players) == 0 ->
          From ! {room_remove, State#state.id}
      end,
      loop(NewState)
  end.

player_remove([{PlayerId, PlayerPID} | XS], PlayerId) ->
  PlayerPID ! stop,
  XS;
player_remove([X | XS], PlayerId) -> lists:append([X], player_remove(XS, PlayerId));
player_remove([], _) -> [].

stop() -> exit(self(), normal).
