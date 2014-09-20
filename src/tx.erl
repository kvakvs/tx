%%%-------------------------------------------------------------------
%%% @doc User frontend for pasting terms
%%% @end
%%% @author Dmytro Lytovchenko <dmytro.lytovchenko@gmail.com>
%%% Created : 19. Sep 2014
%%%-------------------------------------------------------------------
-module(tx).

%% API
-export([show/1]).

show(Term) ->
  case application:start(tx) of
    ok -> ok;
    {error, {already_started, _}} -> ok
  end,

  Id = tx_store:store(Term),
  {ok, TxHost} = application:get_env(tx, host),
  {ok, TxPort} = application:get_env(tx, port),
  Url = lists:flatten(io_lib:format("http://~s:~p/tx/tx_esi:show?~s"
                                   , [TxHost, TxPort, Id])),
  Url.
