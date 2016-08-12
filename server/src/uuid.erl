-module(uuid).

-export([generate/0]).

%% FIXME the UUID is not distributed and unique
generate() ->
  erlang:list_to_bitstring(
    erlang:ref_to_list(
      erlang:make_ref()
    )
  ).
