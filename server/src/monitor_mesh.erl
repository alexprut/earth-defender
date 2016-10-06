-module(monitor_mesh).

%% External exports
-export([start_monitor_mesh/0]).

%% Internal exports
-export([monitor_mesh/0]).

%%% ---------------------------------------------------
%%%
%%% Monitor all mesh nodes.
%%%
%%% ---------------------------------------------------
start_monitor_mesh() ->
  spawn(?MODULE, monitor_mesh, []).

monitor_mesh() ->
  net_kernel:monitor_nodes(true),
  utils:log("Monitoring mesh nodes. ~n", []),
  monitor_mesh_loop().

monitor_mesh_loop() ->
  receive
    {nodeup, Node} ->
      utils:log("Node: ~p is up, detected at node: ~p~n", [Node, node()]),
      monitor_mesh_loop();
    {nodedown, Node} ->
      utils:log("Node: ~p is down, detected at node: ~p~n", [Node, node()]),
      case local_rooms_state:is_master() of
        true ->
          utils:log("Slave died, node: ~p~n", [Node]),
          local_rooms_state:remove_slave(Node);
        false ->
          utils:log("Maybe a master died.~n", []),
          {Master_name, _} = slave_handler:get_master_data(),
          utils:log("Last master name: ~p~n", [Master_name]),
          case Master_name == Node of
            true ->
              % Master died, election algorithm (Bully algorithm modified version)
              utils:log("Master died, node: ~p~n", [Node]),
              New_master = lists:max([node() | nodes()]),
              case New_master == node() of
                true ->
                  utils:log("I'm the new master, takeover.~n", []),
                  local_rooms_state ! master_takeover;
                false ->
                  utils:log("The new master should be: ~p~n", [New_master]),
                  Master_service_url = rpc:call(New_master, utils, get_service_url, [], 2000),
                  case Master_service_url of
                    {badrpc, _} ->
                      utils:log("Badrpc at node: ~p~n", [Node]),
                      ok;
                    _ ->
                      utils:log("Temp master data, the next master node could be: ~p~n", [New_master]),
                      slave_handler:set_master_data({New_master, Master_service_url})
                  end
              end;
            false ->
              % A slave died
              utils:log("A slave died.~n", []),
              ok
          end
      end,
      monitor_mesh_loop();
    stop ->
      exit(kill, self())
  end.
