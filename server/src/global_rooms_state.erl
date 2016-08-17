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
      loop(NewState);
    {room_player_add, RoomId, Player} ->
      RoomPID = search_room_pid(RoomId, State#state.rooms),
      RoomPID ! {player_add, Player},
      loop(State);
    {action_earth_collision, RoomId} ->
      RoomPID = search_room_pid(RoomId, State#state.rooms),
      RoomPID ! {action_earth_collision},
      loop(State)
  end.

search_room_pid(RoomId, [{RoomId, RoomPID} | XS]) -> RoomPID;
search_room_pid(RoomId, [_ | XS]) -> search_room_pid(RoomId, XS);
search_room_pid(RoomId, []) ->
  erlang:display("Warning: there is no such a room of id: "),
  erlang:display(RoomId).

player_remove([{RoomId, RoomPID} | XS], {RoomId, PlayerId}) ->
  RoomPID ! {player_remove, PlayerId},
  XS;
player_remove([X | XS], RP) -> lists:append([X], player_remove(XS, RP));
player_remove([], _) -> [].

room_remove([{RoomId, RoomPID} | XS], RoomId) ->
  RoomPID ! stop,
  XS;
room_remove([X | XS], R) -> lists:append([X], room_remove(XS, R));
room_remove([], _) -> [].
