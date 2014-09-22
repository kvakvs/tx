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
  tx_app:start(),
  Id = tx_store:store(Term),
  {ok, TxHost} = application:get_env(tx, host),
  {ok, TxPort} = application:get_env(tx, port),
  Url = lists:flatten(io_lib:format("http://~s:~p/tx/tx_esi:show?stored=~s"
                                   , [TxHost, TxPort, Id])),
  Url.
