-module(monitors).

-export([start_monitor_slave/1, start_monitor_master/1, master_monitor/1, slave_monitor/1]).

start_monitor_slave(Slave_pid) ->
  spawn(?MODULE, slave_monitor, [Slave_pid]).

slave_monitor(Slave_pid) ->
  Ref = monitor(process, Slave_pid),
  utils:log("Monitoring slave. ~n", []),
  receive
    {'DOWN', Ref, _Type, _Object, Info} ->
      utils:log("Slave died, info: ~p~n", [Info]),
      global_rooms_state:slave_remove(Slave_pid),
      exit(self(), kill)
  end.


start_monitor_master(Master_pid) ->
  spawn(?MODULE, master_monitor, [Master_pid]).

master_monitor(Master_pid) ->
  Ref = monitor(process, Master_pid),
  utils:log("Monitoring master. ~n", []),
  receive
    {'DOWN', Ref, _Type, _Object, Info} ->
      utils:log("Master died, info: ~p~n", [Info]),
      global_rooms_state ! master_takeover
  end.
