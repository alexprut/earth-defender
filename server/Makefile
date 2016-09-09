PROJECT = earth_defender
PROJECT_DESCRIPTION = Earth Defender game server
PROJECT_VERSION = 1.0.0
PROJECT_REGISTERED = global_rooms_state
PROJECT_MOD = earth_defender_app

ERLC_OPTS = +nowarn_export_vars

DEPS = cowboy jiffy
dep_cowboy_commit = master

include erlang.mk

slave: clean rel
	/Users/matrix/workspace/earth-defender/server/_rel/earth_defender/erts-8.0.2/bin/erl -boot /Users/matrix/workspace/earth-defender/server/_rel/earth_defender/releases/1.0.0/earth_defender -sname 'ketchup' -setcookie 'earth_defender' -port 8889 -role slave -master_name earth_defender@PAlexs-MacBook-Air

slave_two: clean rel
	/Users/matrix/workspace/earth-defender/server/_rel/earth_defender/erts-8.0.2/bin/erl -boot /Users/matrix/workspace/earth-defender/server/_rel/earth_defender/releases/1.0.0/earth_defender -sname 'mayonnaise' -setcookie 'earth_defender' -port 8890 -role slave -master_name earth_defender@PAlexs-MacBook-Air
