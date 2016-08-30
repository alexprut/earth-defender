-module(websocket_handler).
-include("config.hrl").

-export([init/2]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/2]).

-record(state, {player_id, room_id}).

init(Req, _Opts) ->
  {cowboy_websocket, Req, #state{}, ?WEBSOCKET_TIMEOUT}.

% Receive/External message Handler
% Client that send messages to the server
websocket_handle({text, Msg}, State) ->
  erlang:display("dentro"),
  % The receiving message Msg is of type <<"[\"event\",data]">>
  io:format("Receiving message:~n~p~n", [jiffy:decode(Msg)]),
  [Event, Data] = jiffy:decode(Msg),
  case binary_to_list(Event) of
    "rooms_list" ->
      global_rooms_state ! {rooms_list, self()},
      reply_ok(State);
    "room_join" ->
      Room_id = Data,
      Player = player:start(self()),
      Player_id = uuid:generate(),
      Player ! {player_id, Player_id},
      New_state = State#state{player_id = Player_id, room_id = Room_id},
      global_rooms_state ! {room_player_add, Room_id = Room_id, {Player_id, Player}},
      global_rooms_state ! {action_new_player_join, New_state#state.room_id},
      reply_ok(New_state);
    "room_add" ->
      Room_id = uuid:generate(),
      io:format("Room id:~n~p~n", [Room_id]),
      Room = room:start(Room_id),
      global_rooms_state ! {room_add, {Room_id, Room}},
      Player = player:start(self()),
      Player_id = uuid:generate(),
      Player ! {player_id, Player_id},
      Room ! {player_add, {Player_id, Player}},
      New_state = State#state{player_id = Player_id, room_id = Room_id},
      reply([<<"room_id">>, Room_id], New_state);
    "action_earth_collision" ->
      global_rooms_state ! {action_earth_collision, State#state.room_id, State#state.player_id},
      reply_ok(State);
    "game_master_asteroids_position" ->
      global_rooms_state ! {game_master_asteroids_position, Data, State#state.room_id},
      reply_ok(State);
    "game_ship_position" ->
      global_rooms_state ! {ship_position, Data, State#state.room_id},
      reply_ok(State);
    "action_ship_move" ->
      global_rooms_state ! {ship_move, Data, State#state.room_id},
      reply_ok(State);
    "action_ship_shoot" ->
      global_rooms_state ! {ship_shoot, Data, State#state.room_id},
      reply_ok(State);
    "ping" ->
      reply([<<"pong">>], State);
    Unknown ->
      io:format("Warning: websocket_handle can not handle event:~n~p~n", [Unknown]),
      reply_ok(State)
  end;
websocket_handle(_Data, State) ->
  {ok, State}.

% Send/Internal message Handler
% Server that send messages to the client
websocket_info({Event, Data}, State) ->
  case Event of
    player_id ->
      reply([<<"player_id">>, Data], State);
    rooms_list ->
      reply([<<"rooms_list">>, Data], State);
    room_players_number ->
      reply([<<"room_players_number">>, Data], State);
    game_life ->
      reply([<<"game_life">>, Data], State);
    asteroid_position ->
      reply([<<"asteroid_position">>, Data], State);
    asteroid_position_set ->
      reply([<<"asteroid_position_set">>, Data], State);
    ship_position_set ->
      reply([<<"ship_position_set">>, Data], State);
    ship_shoot ->
      reply([<<"ship_shoot">>, Data], State);
    Unknown ->
      io:format("Warning: websocket_info can not handle event:~n~p~n", [Unknown]),
      reply_ok(State)
  end.

% Reason con be: remote, crash or normal
% In case the process websocket_handler crashes the terminate function will be executed
terminate({crash, _, _}, _State) ->
  io:format("Warning: process 'websocket_handler' crashed: ~nState:~n~p~n", [_State]),
  global_rooms_state ! {player_remove, {_State#state.room_id, _State#state.player_id}};
terminate(_Reason, _State) ->
  global_rooms_state ! {player_remove, {_State#state.room_id, _State#state.player_id}}.

% Utilities
reply(Data, State) ->
  io:format("Sending message:~n~s~n", [jiffy:encode(Data)]),
  {reply, {text, jiffy:encode(Data)}, State}.
reply_ok(State) -> {ok, State}.
