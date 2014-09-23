%%%-------------------------------------------------------------------
%%% @doc User frontend for pasting terms
%%% @end
%%% @author Dmytro Lytovchenko <dmytro.lytovchenko@gmail.com>
%%% Created : 19. Sep 2014
%%%-------------------------------------------------------------------
-module(tx).

%% API
-export([show/1, show/2, redbug/2, redbug/1]).

%% @doc Stores Term in internal ETS table, starts webserver (if not started)
%% and returns URL where this term can be viewed in browser.
show(Term) -> show(Term, "No name").
show(Term, Title) ->
  tx_app:start(),
  Id = tx_store:store(Term, Title),
  {ok, TxHost} = application:get_env(tx, host),
  {ok, TxPort} = application:get_env(tx, port),
  Url = lists:flatten(io_lib:format("http://~s:~p/tx/tx_esi:show?stored=~s"
                                   , [TxHost, TxPort, Id])),
  Url. % won't be printed!

%% @doc Starts Redbug session (if you have redbug), monitors the process and
%% accumulates values it prints, then after Redbug is finished, accumulated
%% list of trace events is published via show(List, "Redbug trace")
redbug(Spec) -> redbug(Spec, []).
redbug(Spec, Options) ->
  spawn(fun() -> redbug_async(Spec, Options) end).

%%%=============================================================================
%% @private
%% @doc Built-in timeouts guarantee that lifetime of this process won't be
%% longer than redbug timeout times 3.
redbug_async(Spec, Options0) ->
  %% Algorithm:
  %% 1. Spawn accumulator process which will receive printed events (we can't
  %% receive in current process because Redbug receives everything)
  %% 2. Start redbug and wait for it to die or wait for 3xtimeout and kill it
  %% 3. Signal accumulator process that we want results, it will send us results
  %% and exit. If waiting timed out - kill it.
  AccumulatorPid = spawn(fun() -> redbug_accumulate_loop([]) end),

  %% Get old value of print_fun (if was set) and chain it after our print
  PreviousPrinter = proplists:get_value(print_fun, Options0, fun(_) -> ok end),
  MyPrinter = fun(Event) ->
                AccumulatorPid ! {redbug_printout, Event},
                PreviousPrinter(Event)
              end,
  Options = [{print_fun, MyPrinter} | lists:keydelete(print_fun, 1, Options0)],

  %% Starting redbug creates registered 'redbug' process, we wait for it to die
  redbug:start(Spec, Options),
  Ref          = erlang:monitor(process, redbug),
  %% Get redbug timeout and triple it, use as safe kill threshold
  LONG_TIMEOUT = proplists:get_value(time, Options0, 20000) * 3,
  receive
    {'DOWN',Ref,_MonitorT,_MonitorObject,_MonitorInfo} -> ok
  after LONG_TIMEOUT ->
    erlang:exit(erlang:whereis(redbug), kill) % nope
  end,
  %% Signal out accumulator function that its time to yield and die
  AccumulatorPid ! {accumulator_finish, self()},
  receive
    {accumulated_result, Accum} ->
      case Accum of
        [] -> io:format("[tx] nothing from Redbug~n");
        _  -> io:format(show(Accum, "Redbug trace"))
      end
    after 2000 ->
      erlang:exit(AccumulatorPid, kill),
      io:format("[tx] something went wrong. Waiting for accumulated "
                "redbug results timed out~n")
  end.

%% @private
redbug_accumulate_loop(Accum) ->
  receive
    {redbug_printout, X} ->
      redbug_accumulate_loop([X | Accum]);
    {accumulator_finish, Receiver} ->
      Receiver ! {accumulated_result, lists:reverse(Accum)}
  end.