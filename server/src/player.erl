-module(player).

%% External exports
-export([start/3, terminate/1]).

%% Internal exports
-export([loop/1]).

-record(state, {id, ship_id, websocket}).

%%% ---------------------------------------------------
%%%
%%% Player.
%%%
%%% ---------------------------------------------------

start(Websocket, Id, Ship_id) ->
  spawn(player, loop, [#state{websocket = Websocket, id = Id, ship_id = Ship_id}]).

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
    {remove_ship_scene, Ship_id} ->
      Websocket ! {remove_ship_scene, Ship_id},
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
