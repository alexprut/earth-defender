-module(global_rooms_state).

-export([start/0, loop/1]).

% Data in #state.rooms saved as: {room_id, room_pid}
-record(state, {rooms = []}).

start() ->
  register(global_rooms_state, spawn(global_rooms_state, loop, [#state{}])).

loop(State) ->
  receive
    {rooms_list, From} ->
      RoomsList = lists:flatmap(fun(ROOM) -> {ID, _} = ROOM, [ID] end, State#state.rooms),
      erlang:display(RoomsList),
      From ! {rooms_list, RoomsList},
      loop(State);
    {room_add, {RoomId, Room}} ->
      NewState = State#state{rooms = [{RoomId, Room} | State#state.rooms]},
      loop(NewState);
    {player_remove, {RoomId, PlayerId}} ->
      player_remove(State#state.rooms, {RoomId, PlayerId}),
      loop(State);
    {room_remove, RoomId} ->
      NewState = State#state{rooms = room_remove(State#state.rooms, RoomId)},
      loop(NewState)
  end.

player_remove([{RoomId, RoomPID} | XS], {RoomId, PlayerId}) ->
  RoomPID ! {player_remove, PlayerId, self()},
  XS;
player_remove([X | XS], RP) -> lists:append([X], player_remove(XS, RP));
player_remove([], _) -> [].

room_remove([{RoomId, RoomPID} | XS], RoomId) ->
  RoomPID ! stop,
  XS;
room_remove([X | XS], R) -> lists:append([X], room_remove(XS, R));
room_remove([], _) -> [].
