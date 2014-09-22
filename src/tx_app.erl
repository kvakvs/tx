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
  case application:start(tx) of
    ok ->
      start_web(),
      ok;
    {error, {already_started, _}} ->
      ok
  end.

start(_StartType, _StartArgs) ->
  tx_store:start(),
  tx_sup:start_link().

stop(_State) ->
  ok.

%% @doc Call this after tx application started
start_web() ->
  inets:start(),
  {ok, TxPort} = application:get_env(tx, port),
  {ok, ConfHost} = application:get_env(tx, host),
  {ok, TxHost} = inet_parse:address(ConfHost),
  DocRoot = priv_dir(tx),
  SrvRoot = filename:absname(DocRoot ++ "/../"),

  Options = [{modules, [mod_esi
                       , mod_get]}

            , {port, TxPort}
            , {server_name, "127.0.0.1"}
            , {server_root, SrvRoot}
            , {document_root, DocRoot}
            , {bind_address, TxHost}

            , {erl_script_alias, {"/tx", [tx_esi, io]}}

            , {mime_types, [{"html", "text/html"}
                           , {"css", "text/css"}
                           , {"js", "application/x-javascript"}
                           ]}
            , {mime_type, "application/octet-stream"}
            ],
  {ok, Pid} = inets:start(httpd, Options),
  case TxPort of
    0 ->
      Info = httpd:info(Pid),
      {port, ListenPort} = proplists:lookup(port, Info),
      application:set_env(tx, port, ListenPort);
    _ -> ok
  end,

  %% Reread selected port (if was set to 0 in app config)
  {ok, SelectedPort} = application:get_env(tx, port),
  io:format("~n[term explorer] http server started. Add a term using tx:show(Term) or~n", []),
  io:format("[term explorer] visit http://~s:~p/index.html~n"
           , [ConfHost, SelectedPort]).

priv_dir(App) ->
  case code:priv_dir(App) of
    {error, bad_name} ->
      {ok, Cwd} = file:get_cwd(),
      Cwd ++ "/" ++ "priv/";
    Priv ->
      Priv ++ "/"
  end.