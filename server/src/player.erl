-module(player).

-export([start/1, loop/1, terminate/1]).

-record(state, {id, websocket}).

start(Websocket) ->
  spawn(player, loop, [#state{websocket = Websocket}]).

loop(State) ->
  Websocket = State#state.websocket,
  receive
    {player_id, Player_id} ->
      Websocket ! {player_id, Player_id},
      loop(State#state{id = Player_id});
    {websocket, Websocket} ->
      loop(State#state{websocket = Websocket});
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
    stop ->
      terminate(self())
  end.

terminate(PID) -> exit(PID, kill).
