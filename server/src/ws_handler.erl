-module(ws_handler).

-export([init/2]).
-export([websocket_info/3]).

init(Req, Opts) ->
	erlang:start_timer(1000, self(), <<"Hello!">>),
	{cowboy_websocket, Req, Opts}.

websocket_info(_Info, Req, State) ->
	{ok, Req, State}.
