-module(websocket_handler).
-include("config.hrl").

-export([init/2]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

-record(state, {player_id = undef, room_id = undef, room_pid = undef, player_pid = undef, ship_id = undef}).

init(Req, _Opts) ->
  {cowboy_websocket, Req, #state{}, ?WEBSOCKET_TIMEOUT}.

% Receive/External message Handler
% Client that send messages to the server
websocket_handle({text, Msg}, State) ->
  % The receiving message Msg is of type <<"[\"event\",data]">>
  utils:log("Receiving message:~n~p~n", [jiffy:decode(Msg)]),
  [Event, Data] = jiffy:decode(Msg),
  case global_rooms_state:is_master() of
    true ->
      case binary_to_list(Event) of
        "game_reconnect" ->
          [Room_id, Player_id, Ship_id] = Data,
          Room_pid = global_rooms_state:get_room_pid(Room_id),
          case Room_id of
            error ->
              reply([<<"server_error">>], State);
            _ ->
              Player_pid = room:get_player_pid(Room_pid, Player_id),
              Player_pid ! {websocket, self()},
              Room_pid ! {player_reconnect, Player_pid},
              New_state = State#state{player_id = Player_id, room_id = Room_id, player_pid = Player_pid, room_pid = Room_pid},
              self() ! {servers_list, global_rooms_state:get_servers_list()},
              reply([<<"game_reconnect">>], New_state)
          end;
        "rooms_list" ->
          self() ! {servers_list, global_rooms_state:get_servers_list()},
          reply([<<"rooms_list">>, global_rooms_state:get_rooms_list()], State);
        "room_join" ->
          [Room_id, Ship_id] = Data,
          Room_pid = global_rooms_state:get_room_pid(Room_id),
          Player_id = utils:generate_uuid(),
          Player_pid = player:start(self(), Player_id, Ship_id),
          self() ! {player_id, Player_id},
          Room_pid ! {player_add, {Player_id, Player_pid, Ship_id}},
          New_state = State#state{player_id = Player_id, room_id = Room_id, player_pid = Player_pid, room_pid = Room_pid},
          global_rooms_state:init_broadcast_slaves({binary_to_list(Event), {Room_id, Player_id, Ship_id}}),
          reply([<<"room_id">>, Room_id], New_state);
        "room_add" ->
          Ship_id = Data,
          Room_id = utils:generate_uuid(),
          {_, Room_pid} = room:start_link(Room_id),
          utils:log("Room id:~n~p~n", [Room_id]),
          global_rooms_state:add_room(Room_id, Room_pid),
          Player_id = utils:generate_uuid(),
          Player_pid = player:start(self(), Player_id, Ship_id),
          self() ! {player_id, Player_id},
          Room_pid ! {player_add, {Player_id, Player_pid, Ship_id}},
          New_state = State#state{player_id = Player_id, room_id = Room_id, player_pid = Player_pid, room_pid = Room_pid, ship_id = Ship_id},
          global_rooms_state:init_broadcast_slaves({binary_to_list(Event), {Room_id, Player_id, Ship_id}}),
          reply([<<"room_id">>, Room_id], New_state);
        "action_earth_collision" ->
          State#state.room_pid ! {action_earth_collision, State#state.player_id},
          global_rooms_state:init_broadcast_slaves({binary_to_list(Event), {State#state.room_id, State#state.player_id}}),
          reply_ok(State);
        "game_master_asteroids_position" ->
          State#state.room_pid ! {game_master_asteroids_position, Data},
          global_rooms_state:init_broadcast_slaves({binary_to_list(Event), {State#state.room_id, Data}}),
          reply_ok(State);
        "game_ship_position" ->
          State#state.room_pid ! {ship_position, Data},
          global_rooms_state:init_broadcast_slaves({binary_to_list(Event), {State#state.room_id, Data}}),
          reply_ok(State);
        "action_ship_move" ->
          State#state.room_pid ! {ship_move, Data},
          global_rooms_state:init_broadcast_slaves({binary_to_list(Event), {State#state.room_id, Data}}),
          reply_ok(State);
        "action_ship_shoot" ->
          State#state.room_pid ! {ship_shoot, Data},
          global_rooms_state:init_broadcast_slaves({binary_to_list(Event), {State#state.room_id, Data}}),
          reply_ok(State);
        "ping" ->
          reply([<<"pong">>], State);
        Unknown ->
          utils:log("Warning: websocket_handle can not handle event:~n~p~n", [Unknown]),
          reply_ok(State)
      end;
    false ->
      reply([<<"servers_list_redirect">>, global_rooms_state:get_servers_list()], State)
  end;
websocket_handle(_Data, State) ->
  {ok, State}.

% Send/Internal message Handler
% Server that send messages to the client
websocket_info({Event, Data}, State) ->
  case Event of
    player_id ->
      reply([<<"player_id">>, Data], State);
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
    remove_ship_scene ->
      reply([<<"remove_ship_scene">>, Data], State);
    servers_list ->
      reply([<<"servers_list">>, Data], State);
    Unknown ->
      utils:log("Warning: websocket_info can not handle event:~n~p~n", [Unknown]),
      reply_ok(State)
  end.

% Reason con be: remote, crash or normal
% In case the process websocket_handler crashes the terminate function will be executed
terminate({crash, _, _}, _Req, State) ->
  utils:log("Warning: process 'websocket_handler' crashed: ~nState:~n~p~nTrace:~n~p~n", [State, erlang:get_stacktrace()]),
  if
    State#state.room_pid == undef ->
      ok;
    true ->
      State#state.room_pid ! {player_remove, State#state.player_id} % TODO tell slaves to remove player
  end;
terminate(_Reason, _Req, State) ->
  utils:log("Terminating websocket~n", []),
  if
    State#state.room_pid == undef ->
      ok;
    true ->
      State#state.room_pid ! {player_remove, State#state.player_id} % TODO tell slaves to remove player
  end.

% Utilities
reply(Data, State) ->
  utils:log("Sending message:~n~s~n", [jiffy:encode(Data)]),
  {reply, {text, jiffy:encode(Data)}, State}.
reply_ok(State) -> {ok, State}.
