%%%-------------------------------------------------------------------
%%% @doc Erlang Server Interface (ESI) module doing content generation
%%% @end
%%% @author Dmytro Lytovchenko <dmytro.lytovchenko@gmail.com>
%%% Created : 19. Sep 2014
%%%-------------------------------------------------------------------
-module(tx_esi).

%% API
-export([show/3]).

show(Sid, _Env, _In) ->
%%   try
    Json = tx_term:to_json(erlang:process_info(self())),
%%     io:format("~p~n", [Json]),
    Resp = tx_mochijson2:encode(Json),
    mod_esi:deliver(Sid, Resp).
%%   catch T:E ->
%%     io:format("~p:~p ~p~n", [T, E, erlang:get_stacktrace()])
%%   end.
