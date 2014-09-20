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
%%   application:start(sasl),
  application:start(tx).

start(_StartType, _StartArgs) ->
  start_web(),
  tx_store:start(),
  tx_sup:start_link().

stop(_State) ->
  ok.

start_web() ->
  inets:start(),
  {ok, TxPort}   = application:get_env(tx, port),
  {ok, ConfHost} = application:get_env(tx, host),
  {ok, TxHost}   = inet_parse:address(ConfHost),
  DocRoot = filename:absname("priv/"),
  SrvRoot = filename:absname(""),

  Options    = [ {modules, [ mod_esi
                           , mod_get ]}
               %, {debug, {all_functions, [tx_esi]}}
%%                , {directory, {DocRoot, [ {directory_index, ["index.html"]}
%%                                        ]}}

               , {port, TxPort}
               , {server_name, "127.0.0.1"}
               , {server_root, SrvRoot}
               , {document_root, DocRoot}
               , {bind_address, TxHost}

               , {erl_script_alias, {"/tx", [tx_esi, io]}}

               , {mime_types, [ {"html", "text/html"}
                              , {"css", "text/css"}
                              , {"js", "application/x-javascript"}
                              ]}
               , {mime_type, "application/octet-stream"}
               ],
  {ok, _Pid} = inets:start(httpd, Options),
  io:format("~n[term explorer] http server started. Add a term using tx:show(Term) or~n", []),
  io:format("[term explorer] visit http://~s:~p/index.html~n", [ConfHost, TxPort]).
