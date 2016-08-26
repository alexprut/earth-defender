-module(room).
-include("config.hrl").

-export([start/1, loop/1, terminate/1]).

% Data in #state.players saved as: {player_id, player_pid}
-record(state, {players = [], id, life = ?EARTH_LIFE, a_position, s_position = [[]]}).

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
    {action_earth_collision, Pid} ->
%      X = lists:last(State#state.players),
%      [{Pid,_}|_] = X,
      NewLife = State#state.life - 200,
%      if
%        Pid == X -> NewLife = State#state.life - 200;
%        NewLife -> NewLife = State#state.life
%      end,
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
      loop(NewState);
    {ship_position, PositionShip} ->
      PositionList = [PositionShip|State#state.s_position],
      NewState = State#state{s_position = PositionList},
      broadcast(State#state.players, ship_position_set, PositionList),
      loop(NewState);
    {ship_move, [IdShip,Direction]} ->
      NewState = State#state{s_position = update_position(State#state.s_position,IdShip,Direction)},
      broadcast(State#state.players, ship_position_set, State#state.s_position),
      loop(NewState);
    {ship_shoot, IdShip} ->
      broadcast(State#state.players, ship_shoot, IdShip),
      loop(State)
  end.

update_position([[IdShip,X,Y,Z]|XS],IdShip,Direction)->
    case binary_to_list(Direction) of
        "x+" -> [[IdShip,X+2,Y,Z]|XS];
        "x-" -> [[IdShip,X-2,Y,Z]|XS];
        "y+" -> [[IdShip,X,Y+2,Z]|XS];
        "y-" -> [[IdShip,X,Y-2,Z]|XS];
        "z+" -> [[IdShip,X,Y,Z+5]|XS];
        "z-" -> [[IdShip,X,Y,Z-5]|XS]
    end;
update_position([X|XS],IdShip,Direction) -> lists:append([X],update_position(XS, IdShip, Direction));
update_position([],_,_) -> [].

player_remove([{PlayerId, PlayerPID} | XS], PlayerId) ->
  PlayerPID ! stop,
  XS;
player_remove([X | XS], PlayerId) -> lists:append([X], player_remove(XS, PlayerId));
player_remove([], _) -> [].

broadcast([], _, _) -> ok;
broadcast([{_, Player_pid} | XS], Event, Data) ->
  Player_pid ! {Event, Data},
  broadcast(XS, Event, Data).

terminate(PID) -> exit(PID, kill).
