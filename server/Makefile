PROJECT = earth_defender
PROJECT_DESCRIPTION = Earth Defender game server
PROJECT_VERSION = 1.0.0
PROJECT_REGISTERED = global_rooms_state
PROJECT_MOD = earth_defender_app

ERLC_OPTS = +nowarn_export_vars

DEPS = cowboy jiffy
dep_cowboy_commit = master

include erlang.mk

# Example
slave: clean rel
	./_rel/earth_defender/erts-8.0.2/bin/erl -boot ./_rel/earth_defender/releases/1.0.0/earth_defender -sname 'ketchup' -setcookie 'earth_defender' -port 8889 -hostname "localhost" -role slave -master_name earth_defender@host
