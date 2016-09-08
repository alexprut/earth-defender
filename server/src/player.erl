-module(player).

-export([start/2, loop/1, terminate/1]).

-record(state, {id, websocket}).

start(Websocket, Id) ->
  spawn(player, loop, [#state{websocket = Websocket, id = Id}]).

loop(State) ->
  Websocket = State#state.websocket,
  receive
    {player_id, Player_id} ->
      Websocket ! {player_id, Player_id},
      loop(State#state{id = Player_id});
    {websocket, New_websocket} ->
      loop(State#state{websocket = New_websocket});
    {room_players_number, Players_number} ->
      Websocket ! {room_players_number, Players_number},
      loop(State);
    {game_life, Life} ->
      Websocket ! {game_life, Life},
      loop(State);
    {asteroid_position, _} ->
      Websocket ! {asteroid_position, null},
      loop(State);
    {asteroid_position_set, Position} ->
      Websocket ! {asteroid_position_set, Position},
      loop(State);
    {ship_position_set, Ship_position} ->
      Websocket ! {ship_position_set, Ship_position},
      loop(State);
    {ship_shoot, Ship_id} ->
      Websocket ! {ship_shoot, Ship_id},
      loop(State);
    {servers_list, Data} ->
      Websocket ! {servers_list, Data},
      loop(State);
    stop ->
      terminate(self());
    Unknown ->
      utils:log("Warning: unknown message received in 'player:loop', message: ~p~n", [Unknown]),
      loop(State)
  end.

terminate(PID) ->
  utils:log("Killed: player, pid: ~p~n", [PID]),
  exit(PID, kill).
