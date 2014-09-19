%%%-------------------------------------------------------------------
%%% @doc Erlang Server Interface (ESI) module doing content generation
%%% @end
%%% @author Dmytro Lytovchenko <dmytro.lytovchenko@gmail.com>
%%% Created : 19. Sep 2014
%%%-------------------------------------------------------------------
-module(tx_esi).

%% API
-export([show/3, list/3, delete/3]).

show(Sid, Env, _In) ->
  Id = list_to_binary(proplists:get_value(query_string, Env)),
  Json = tx_term:to_json(tx_store:read(Id)),
  Resp = tx_mochijson2:encode(Json),
  mod_esi:deliver(Sid, Resp).

delete(Sid, Env, _In) ->
  Id = list_to_binary(proplists:get_value(query_string, Env)),
  tx_store:delete(Id),
  mod_esi:deliver(Sid, <<"ok">>).

list(Sid, _Env, _In) ->
  Json = tx_store:list_json(),
  Resp = tx_mochijson2:encode(Json),
  mod_esi:deliver(Sid, Resp).
