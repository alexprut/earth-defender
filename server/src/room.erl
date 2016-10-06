-module(room).

-behavior(gen_server).

-include("config.hrl").
-include("room_state.hrl").

%% External exports
-export([start_link/1, get_player_pid/2, create_state_snapshot/1, set_state/2, stop/1]).

%% Internal exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%% ---------------------------------------------------
%%%
%%% gen_server.
%%%
%%% ---------------------------------------------------

start_link(Room_id) ->
  gen_server:start_link(?MODULE, Room_id, []).

init(_Room_id) ->
  {ok, #room_state{id = _Room_id, life = ?EARTH_LIFE}}.

% Synchronous messages
handle_call(Request, _From, State) ->
  case Request of
    create_state_snapshot ->
      utils:log("Master local state room snapshot: ~n~p~n", [State]),
      {reply, State, State};
    {find_player_pid, Player_id} ->
      utils:log("Searching player of pid: ~p, available players: ~p~n", [Player_id, State#room_state.players]),
      Player_pid = find_player_pid(State#room_state.players, Player_id),
      case Player_pid of
        error -> {reply, error, State};
        _ -> {reply, Player_pid, State}
      end;
    Unknown ->
      utils:log("Warning: 'room:handle_call' can't handle request: ~p~n", [Unknown]),
      {reply, ok, State}
  end.

% Asynchronous messages
handle_cast(Request, State) ->
  case Request of
    stop ->
      utils:log("Killing: room, pid: ~p~n", [self()]),
      {stop, normal, State};
    {set_state, New_state} ->
      utils:log("Updating room state to: ~n~p~n", [New_state]),
      Players = lists:flatmap(
        fun({Player_id, _, Ship_id}) ->
          [{
            Player_id,
            player:start(whereis(slave_handler), Player_id, Ship_id),
            Ship_id
          }]
        end,
        New_state#room_state.players
      ),
      {noreply, New_state#room_state{players = Players}};
    Unknown ->
      utils:log("Warning: 'room:handle_cast' can't handlee request: ~p~n", [Unknown]),
      {noreply, State}
  end.

% Asynchronous messages
handle_info(Data, State) ->
  case Data of
    {broadcast_players, Msg} ->
      lists:flatmap(fun({_, Player_pid, _}) -> Player_pid ! Msg, [] end, State#room_state.players),
      {noreply, State};
    {room_id, Room_id} ->
      {noreply, State#room_state{id = Room_id}};
    {player_remove, Player_id} ->
      Ship_id = find_ship_id(State#room_state.players, Player_id),
      New_ship_positions = remove_ship_from_list(
        State#room_state.ships_position,
        Ship_id
      ),
      New_state = State#room_state{
        players = remove_player(State#room_state.players, Player_id),
        ships_position = New_ship_positions
      },
      if
        length(New_state#room_state.players) == 0 ->
          local_rooms_state ! {room_remove, State#room_state.id};
        true ->
          broadcast(New_state#room_state.players, room_players_number, length(New_state#room_state.players)),
          broadcast(New_state#room_state.players, remove_ship_scene, Ship_id)
      end,
      {noreply, New_state};
    {player_reconnect, Player_pid} ->
      Player_pid ! {game_life, State#room_state.life},
      broadcast(State#room_state.players, room_players_number, length(State#room_state.players)),
      if
        length(State#room_state.players) > 1 ->
          broadcast([lists:last(State#room_state.players)], asteroid_position, []);
        true -> ok
      end,
      {noreply, State};
    {player_add, {Player_id, Player_pid, Ship_id}} ->
      New_state = State#room_state{players = [{Player_id, Player_pid, Ship_id} | State#room_state.players]},
      Player_pid ! {game_life, New_state#room_state.life},
      broadcast(New_state#room_state.players, room_players_number, length(New_state#room_state.players)),
      if
        length(New_state#room_state.players) > 1 ->
          broadcast([lists:last(State#room_state.players)], asteroid_position, []);
        true -> ok
      end,
      {noreply, New_state};
    {action_earth_collision, Player_id} ->
      utils:log("Earth collission, player_id: ~p, players: ~p~n", [Player_id, State#room_state.players]),
      [{P_id, _, _} | _] = State#room_state.players,
      if
        (P_id == Player_id) -> Tmp_life = State#room_state.life - ?EARTH_LIFE_DECREASE;
        true -> Tmp_life = State#room_state.life
      end,
      if
        Tmp_life >= 0 -> New_life = Tmp_life;
        Tmp_life -> New_life = 0
      end,
      New_state = State#room_state{life = New_life},
      broadcast(State#room_state.players, game_life, New_life),
      {noreply, New_state};
    {game_master_asteroids_position, Position} ->
      New_state = State#room_state{asteroids_position = Position},
      broadcast(State#room_state.players, asteroid_position_set, Position),
      {noreply, New_state};
    {ship_position, Position_ship} ->
      Position_list = [Position_ship | State#room_state.ships_position],
      New_state = State#room_state{ships_position = Position_list},
      broadcast(State#room_state.players, ship_position_set, Position_list),
      {noreply, New_state};
    {ship_move, [Ship_id, Direction]} ->
      New_state = State#room_state{
        ships_position = update_position(State#room_state.ships_position, Ship_id, Direction)
      },
      broadcast(State#room_state.players, ship_position_set, New_state#room_state.ships_position),
      {noreply, New_state};
    {ship_shoot, Ship_id} ->
      broadcast(State#room_state.players, ship_shoot, Ship_id),
      {noreply, State};
    % FIXME remove me, and check where it is used
    stop ->
      terminate(self()),
      {noreply, State};
    Unknown ->
      utils:log("Warning: unknown message received in 'room:handle_info', message: ~p~n", [Unknown]),
      {noreply, State}
  end.

terminate(Reason, State) ->
  utils:log("Terminating 'room', reason: ~p~n", [Reason]),
  lists:flatmap(
    fun({_, Player_pid, _}) ->
      Player_pid ! stop,
      []
    end,
    State#room_state.players
  ),
  ok.

% FIXME remove me, and check where it is used
terminate(PID) ->
  utils:log("Killed: room, pid: ~p~n", [PID]),
  exit(PID, kill).

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%% ---------------------------------------------------
%%%
%%% gen_server calls: utilities functions.
%%%
%%% ---------------------------------------------------

stop(Room_pid) ->
  gen_server:cast(Room_pid, stop).

get_player_pid(Room_pid, Player_id) ->
  gen_server:call(Room_pid, {find_player_pid, Player_id}).

create_state_snapshot(Room_pid) ->
  utils:log("Creating local master snapshot of room with pid: ~p~n", [Room_pid]),
  gen_server:call(Room_pid, create_state_snapshot).

set_state(Room_pid, State) ->
  gen_server:cast(Room_pid, {set_state, State}).

broadcast([], _, _) -> ok;
broadcast([{_, Player_pid, _} | XS], Event, Data) ->
  Player_pid ! {Event, Data},
  broadcast(XS, Event, Data).

%%% ---------------------------------------------------
%%%
%%% Utilities functions.
%%%
%%% ---------------------------------------------------

update_position([[Ship_id, X, Y, Z] | XS], Ship_id, Direction) ->
  case binary_to_list(Direction) of
    "x+" -> [[Ship_id, X + ?SHIP_MOVE_FACTOR_X, Y, Z] | XS];
    "x-" -> [[Ship_id, X - ?SHIP_MOVE_FACTOR_X, Y, Z] | XS];
    "y+" -> [[Ship_id, X, Y + ?SHIP_MOVE_FACTOR_Y, Z] | XS];
    "y-" -> [[Ship_id, X, Y - ?SHIP_MOVE_FACTOR_Y, Z] | XS];
    "z+" -> [[Ship_id, X, Y, Z + ?SHIP_MOVE_FACTOR_Z] | XS];
    "z-" -> [[Ship_id, X, Y, Z - ?SHIP_MOVE_FACTOR_Z] | XS]
  end;
update_position([X | XS], Ship_id, Direction) ->
  lists:append([X], update_position(XS, Ship_id, Direction));
update_position([], Ship_id, _) ->
  utils:log("Warning: there is no such a ship with ship_id: ~p~n", [Ship_id]),
  error.

find_player_pid(Players, Player_id) ->
  Match = lists:keyfind(Player_id, 1, Players),
  case Match of
    {Player_id, Player_pid, _} ->
      Player_pid;
    _ ->
      utils:log("Warning: there is no such a player with id: ~p~n", [Player_id]),
      error
  end.

find_ship_id(Players, Player_id) ->
  Match = lists:keyfind(Player_id, 1, Players),
  case Match of
    {Player_id, _, Ship_id} ->
      Ship_id;
    _ ->
      utils:log("Warning: there is no such a ship with player_id: ~p~n", [Player_id]),
      error
  end.

remove_player([{Player_id, Player_pid, _} | XS], Player_id) ->
  Player_pid ! stop,
  XS;
remove_player([X | XS], Player_id) -> lists:append([X], remove_player(XS, Player_id));
remove_player([], Player_id) ->
  utils:log("Warning: there is no such a player with id: ~p~n", [Player_id]),
  error.

remove_ship_from_list([[Ship_id, _, _, _] | XS], Ship_id) ->
  XS;
remove_ship_from_list([X | XS], Ship_id) -> lists:append([X], remove_ship_from_list(XS, Ship_id));
remove_ship_from_list([], Ship_id) ->
  utils:log("Warning: there is no such a ship with id: ~p~n", [Ship_id]),
  error.
