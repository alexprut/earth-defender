-module(ws_handler).

-export([init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([terminate/3]).

-record(state, {player_id, room_id}).

init(Req, Opts) ->
  {cowboy_websocket, Req, #state{}, 120000}.

% Receive/External message Handler
% Client that send messages to the server
websocket_handle({text, Msg}, Req, State) ->
  % The receiving message Msg is of type <<"[\"event\",data]">>
  erlang:display("Receiving message:"),
  erlang:display(jiffy:decode(Msg)),
  [Event, Data] = jiffy:decode(Msg),
  case binary_to_list(Event) of
    "rooms_list" ->
      global_rooms_state ! {rooms_list, self()},
      reply_ok(Req, State);
    "room_join" ->
      Player = player:start(self()),
      PlayerId = uuid:generate(),
      Player ! {player_id, PlayerId},
      RoomId = Data,
      NewState = State#state{player_id = PlayerId, room_id = RoomId},
      global_rooms_state ! {room_player_add, RoomId = RoomId, {PlayerId, Player}},
      reply_ok(Req, NewState);
    "room_add" ->
      RoomId = uuid:generate(),
      erlang:display("Room id:"),
      erlang:display(RoomId),
      % Create new Room
      Room = room:start(RoomId),
      global_rooms_state ! {room_add, {RoomId, Room}},
      Player = player:start(self()),
      PlayerId = uuid:generate(),
      Player ! {player_id, PlayerId},
      Room ! {player_add, {PlayerId, Player}},
      NewState = State#state{player_id = PlayerId, room_id = RoomId},
      reply([<<"room_id">>, RoomId], Req, NewState);
    "player_add" ->
      reply_ok(Req, State);
    "player_remove" ->
      {reply, {text, State}, Req, State};
    "players_get" ->
      {reply, {text, State}, Req, State};
    Default ->
      erlang:display("Warning: websocket_handle can not handle event:"),
      erlang:display(Default),
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
    Default ->
      erlang:display("Warning: websocket_info can not handle event:"),
      erlang:display(Default),
      reply_ok(Req, State)
  end.

terminate(_Reason, _Req, _State) ->
  global_rooms_state ! {player_remove, {_State#state.room_id, _State#state.player_id}}.

% Utilities
reply(Data, Req, State) ->
  erlang:display("Sending message:"),
  erlang:display(jiffy:encode(Data)),
  {reply, {text, jiffy:encode(Data)}, Req, State}.
reply_ok(Req, State) -> {ok, Req, State}.
