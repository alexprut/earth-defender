-module(player).

-export([start/1, loop/1, terminate/1]).

-record(state, {id, websocket}).

start(WebSocket) ->
  spawn(player, loop, [#state{websocket = WebSocket}]).

loop(State) ->
  WebSocket = State#state.websocket,
  receive
    {player_id, PlayerId} ->
      WebSocket ! {player_id, PlayerId},
      loop(State#state{id = PlayerId});
    {websocket, WebSocket} ->
      loop(State#state{websocket = WebSocket});
    {room_players_number, Players_number} ->
      WebSocket ! {room_players_number, Players_number},
      loop(State);
    {game_life, Life} ->
      WebSocket ! {game_life, Life},
      loop(State);
    {asteroid_position, _} ->
      WebSocket ! {asteroid_position, null},
      loop(State);
    {asteroid_position_set, Position} ->
      WebSocket ! {asteroid_position_set, Position},
      loop(State);
    {ship_position_set, PositionShip} ->
      WebSocket ! {ship_position_set, PositionShip},
      loop(State);
    {ship_shoot, IdShip} ->
      WebSocket ! {ship_shoot, IdShip},
      loop(State);
    stop ->
      terminate(self())
  end.

terminate(PID) -> exit(PID, kill).
