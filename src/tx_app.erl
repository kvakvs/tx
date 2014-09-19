%%%-------------------------------------------------------------------
%%% @doc Application startup module
%%% @end
%%% @author Dmytro Lytovchenko <dmytro.lytovchenko@gmail.com>
%%% Created : 19. Sep 2014
%%%-------------------------------------------------------------------
-module(tx_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, start_web/0, start/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
  application:start(tx).

start(_StartType, _StartArgs) ->
  start_web(),
  tx_sup:start_link().

stop(_State) ->
  ok.

start_web() ->
  inets:start(),
  RootDir    = "./priv/",
  TxPort     = 20000,
  TxHost     = {127, 0, 0, 1},
  Options    = [ {port, TxPort}
               , {server_name, "127.0.0.1"}
               , {server_root, filename:absname(RootDir ++ "/..")}
               , {document_root, filename:absname(RootDir)}
               , {bind_address, TxHost}
               , {modules, [mod_esi]}
               , {erl_script_alias, {"/tx", [tx_esi, io]}}
               ],
  {ok, _Pid} = inets:start(httpd, Options),
  io:format("[term explorer] http started on localhost (port ~p)~n", [TxPort]).
