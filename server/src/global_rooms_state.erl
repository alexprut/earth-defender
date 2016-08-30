-module(global_rooms_state).
-behavior(gen_server).

-export([handle_info/2, start_link/0, init/1, handle_call/3, handle_cast/2, code_change/3, terminate/2]).

% Data in #state.rooms saved as: {room_id, room_pid}
-record(state, {rooms = []}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
  {ok, #state{}}.

handle_info(Info, State) ->
  case Info of
    {rooms_list, From} ->
      Rooms_list = lists:flatmap(fun(Room) -> {Id, _} = Room, [Id] end, State#state.rooms),
      io:format("Rooms list: ~p~n", [Rooms_list]),
      From ! {rooms_list, Rooms_list},
      {noreply, State};
    {room_add, {Room_id, Room}} ->
      New_state = State#state{rooms = [{Room_id, Room} | State#state.rooms]},
      {noreply, New_state};
    {player_remove, {Room_id, Player_id}} ->
      player_remove(State#state.rooms, {Room_id, Player_id}),
      {noreply, State};
    {room_remove, Room_id} ->
      New_state = State#state{rooms = room_remove(State#state.rooms, Room_id)},
      {noreply, New_state};
    {room_player_add, Room_id, Player} ->
      Room_pid = search_room_pid(Room_id, State#state.rooms),
      Room_pid ! {player_add, Player},
      {noreply, State};
    {action_earth_collision, Room_id, Player_id} ->
      Room_pid = search_room_pid(Room_id, State#state.rooms),
      Room_pid ! {action_earth_collision, Player_id},
      {noreply, State};
    {action_new_player_join, Room_id} ->
      Room_pid = search_room_pid(Room_id, State#state.rooms),
      Room_pid ! {action_new_player_join},
      {noreply, State};
    {game_master_asteroids_position, Data, Room_id} ->
      Room_pid = search_room_pid(Room_id, State#state.rooms),
      Room_pid ! {game_master_asteroids_position, Data},
      {noreply, State};
    {ship_position, Data, Room_id} ->
      Room_pid = search_room_pid(Room_id, State#state.rooms),
      Room_pid ! {ship_position, Data},
      {noreply, State};
    {ship_move, Data, Room_id} ->
      Room_pid = search_room_pid(Room_id, State#state.rooms),
      Room_pid ! {ship_move, Data},
      {noreply, State};
    {ship_shoot, Data, Room_id} ->
      Room_pid = search_room_pid(Room_id, State#state.rooms),
      Room_pid ! {ship_shoot, Data},
      {noreply, State};
    Unknown ->
      io:format("Warning: unknown message received in 'global_room_state', message: ~p~n", [Unknown]),
      {noreply, State}
  end.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

search_room_pid(Room_id, [{Room_id, Room_pid} | _]) -> Room_pid;
search_room_pid(Room_id, [_ | XS]) -> search_room_pid(Room_id, XS);
search_room_pid(Room_id, []) ->
  io:format("Warning: there is no such a room of id: ~p~n", [Room_id]).

player_remove([{Room_id, Room_pid} | XS], {Room_id, Player_id}) ->
  Room_pid ! {player_remove, Player_id},
  XS;
player_remove([X | XS], RP) -> lists:append([X], player_remove(XS, RP));
player_remove([], _) -> [].

room_remove([{Room_id, Room_pid} | XS], Room_id) ->
  Room_pid ! stop,
  XS;
room_remove([X | XS], R) -> lists:append([X], room_remove(XS, R));
room_remove([], _) -> [].
