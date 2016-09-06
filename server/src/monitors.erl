-module(monitors).

-export([start_monitor_slave/1, start_monitor_master/1, master_monitor/1, slave_monitor/1]).

start_monitor_slave(Slave_pid) ->
  spawn(?MODULE, slave_monitor, [Slave_pid]).

slave_monitor(Slave_pid) ->
  Ref = monitor(process, Slave_pid),
  receive
    {'DOWN', Ref, Type, Object, Info} ->
      exit(self(), kill)
  end.


start_monitor_master(Master_pid) ->
  spawn(?MODULE, master_monitor, [Master_pid]).

master_monitor(Master_pid) ->
  Ref = monitor(process, Master_pid),
  utils:log("Monitoring master. ~n", []),
  receive
    {'DOWN', Ref, Type, Object, Info} ->
      erlang:display("Master died"),
      erlang:display("global_rooms_state_opid"),
      erlang:display(whereis(global_rooms_state)),
      global_rooms_state ! master_takeover
  end.
