-module(player).

-export([start/1, loop/1]).

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
    {asteroid_position, Position} ->
      WebSocket ! {asteroid_position, Position},
      loop(State);
    stop ->
      stop()
  end.

stop() -> exit(self(), normal).
