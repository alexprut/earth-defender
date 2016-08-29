-module(global_rooms_state).
-behavior(gen_server).

-export([handle_info/2, start_link/0, init/1]).

% Data in #state.rooms saved as: {room_id, room_pid}
-record(state, {rooms = []}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(Args) ->
  {ok, #state{}}.

handle_info(Info, State) ->
  case Info of
    {rooms_list, From} ->
      RoomsList = lists:flatmap(fun(ROOM) -> {ID, _} = ROOM, [ID] end, State#state.rooms),
      erlang:display(RoomsList),
      io:format("Rooms list: ~p~n", [RoomsList]),
      From ! {rooms_list, RoomsList},
      {noreply, State};
    {room_add, {RoomId, Room}} ->
      NewState = State#state{rooms = [{RoomId, Room} | State#state.rooms]},
      {noreply, NewState};
    {player_remove, {RoomId, PlayerId}} ->
      player_remove(State#state.rooms, {RoomId, PlayerId}),
      {noreply, State};
    {room_remove, RoomId} ->
      NewState = State#state{rooms = room_remove(State#state.rooms, RoomId)},
      {noreply, NewState};
    {room_player_add, RoomId, Player} ->
      RoomPID = search_room_pid(RoomId, State#state.rooms),
      RoomPID ! {player_add, Player},
      {noreply, State};
    {action_earth_collision, RoomId, PlayerId} ->
      RoomPID = search_room_pid(RoomId, State#state.rooms),
      RoomPID ! {action_earth_collision, PlayerId},
      {noreply, State};
    {action_new_player_join, RoomId} ->
      RoomPID = search_room_pid(RoomId, State#state.rooms),
      RoomPID ! {action_new_player_join},
      {noreply, State};
    {master_asteroid_position, Data, RoomId} ->
      RoomPID = search_room_pid(RoomId, State#state.rooms),
      RoomPID ! {master_asteroid_position, Data},
      {noreply, State};
    {ship_position, Data, RoomId} ->
      RoomPID = search_room_pid(RoomId, State#state.rooms),
      RoomPID ! {ship_position, Data},
      {noreply, State};
    {ship_move, Data, RoomId} ->
      RoomPID = search_room_pid(RoomId, State#state.rooms),
      RoomPID ! {ship_move, Data},
      {noreply, State};
    {ship_shoot, Data, RoomId} ->
      RoomPID = search_room_pid(RoomId, State#state.rooms),
      RoomPID ! {ship_shoot, Data},
      {noreply, State};
    Unknown ->
      io:format("Warning: unknown message received in 'global_room_state', message: ~p~n", [Unknown]),
      {noreply, State}
  end.

search_room_pid(RoomId, [{RoomId, RoomPID} | XS]) -> RoomPID;
search_room_pid(RoomId, [_ | XS]) -> search_room_pid(RoomId, XS);
search_room_pid(RoomId, []) ->
  io:format("Warning: there is no such a room of id: ~p~n", [RoomId]).

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
