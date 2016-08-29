-module(websocket_handler).
-include("config.hrl").

-export([init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([terminate/3]).

-record(state, {player_id, room_id}).

init(Req, Opts) ->
  {cowboy_websocket, Req, #state{}, ?WEBSOCKET_TIMEOUT}.

% Receive/External message Handler
% Client that send messages to the server
websocket_handle({text, Msg}, Req, State) ->
  % The receiving message Msg is of type <<"[\"event\",data]">>
  io:format("Receiving message:~n~p~n", [jiffy:decode(Msg)]),
  [Event, Data] = jiffy:decode(Msg),
  case binary_to_list(Event) of
    "rooms_list" ->
      global_rooms_state ! {rooms_list, self()},
      reply_ok(Req, State);
    "room_join" ->
      Player = player:start(self()),
      Player_id = uuid:generate(),
      Player ! {player_id, Player_id},
      Room_id = Data,
      New_state = State#state{player_id = Player_id, room_id = Room_id},
      global_rooms_state ! {room_player_add, Room_id = Room_id, {Player_id, Player}},
      reply_ok(Req, New_state);
    "room_add" ->
      Room_id = uuid:generate(),
      io:format("Room id:~n~p~n", [Room_id]),
      % Create new Room
      Room = room:start(Room_id),
      global_rooms_state ! {room_add, {Room_id, Room}},
      Player = player:start(self()),
      Player_id = uuid:generate(),
      Player ! {player_id, Player_id},
      Room ! {player_add, {Player_id, Player}},
      New_state = State#state{player_id = Player_id, room_id = Room_id},
      reply([<<"room_id">>, Room_id], Req, New_state);
    "player_add" ->
      reply_ok(Req, State);
    "player_remove" ->
      {reply, {text, State}, Req, State};
    "players_get" ->
      {reply, {text, State}, Req, State};
    "action_earth_collision" ->
      global_rooms_state ! {action_earth_collision, State#state.room_id, State#state.player_id},
      reply_ok(Req, State);
    "action_new_player_join" ->
      global_rooms_state ! {action_new_player_join, State#state.room_id},
      reply_ok(Req, State);
    "master_asteroid_position" ->
      Vector3 = Data,
      global_rooms_state ! {master_asteroid_position, Vector3, State#state.room_id},
      reply_ok(Req, State);
    "ship_position" ->
      Vector3 = Data,
      global_rooms_state ! {ship_position, Vector3, State#state.room_id},
      reply_ok(Req, State);
    "ship_move" ->
      Direction = Data,
      global_rooms_state ! {ship_move, Direction, State#state.room_id},
      reply_ok(Req, State);
    "ship_shoot" ->
      global_rooms_state ! {ship_shoot, Data, State#state.room_id},
      reply_ok(Req, State);
    "ping" ->
      reply([<<"pong">>], Req, State);
    Unknown ->
      io:format("Warning: websocket_handle can not handle event:~n~p~n", [Unknown]),
      reply_ok(Req, State)
  end;
websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.

% Send/Internal message Handler
% Server that send messages to the client
websocket_info({Event, Data}, Req, State) ->
  case Event of
    player_id ->
      reply([<<"player_id">>, Data], Req, State);
    rooms_list ->
      reply([<<"rooms_list">>, Data], Req, State);
    room_players_number ->
      reply([<<"room_players_number">>, Data], Req, State);
    game_life ->
      reply([<<"game_life">>, Data], Req, State);
    asteroid_position ->
      reply([<<"asteroid_position">>, Data], Req, State);
    asteroid_position_set ->
      reply([<<"asteroid_position_set">>, Data], Req, State);
    ship_position_set ->
      reply([<<"ship_position_set">>, Data], Req, State);
    ship_shoot ->
      reply([<<"ship_shoot">>, Data], Req, State);
    Unknown ->
      io:format("Warning: websocket_info can not handle event:~n~p~n", [Unknown]),
      reply_ok(Req, State)
  end.

% Reason con be: remote, crash or normal
% In case the process ws_handler crashes the terminate function will be executeds
terminate({crash, _, _}, _Req, _State) ->
  io:format("Warning: process 'ws_handler' crashed"),
  global_rooms_state ! {player_remove, {_State#state.room_id, _State#state.player_id}};
terminate(_Reason, _Req, _State) ->
  global_rooms_state ! {player_remove, {_State#state.room_id, _State#state.player_id}}.

% Utilities
reply(Data, Req, State) ->
  io:format("Sending message:~n~s~n", [jiffy:encode(Data)]),
  {reply, {text, jiffy:encode(Data)}, Req, State}.
reply_ok(Req, State) -> {ok, Req, State}.
