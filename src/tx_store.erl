%%%-------------------------------------------------------------------
%%% @doc Stores terms in ETS table. Cleans records eventually
%%% @end
%%% @author Dmytro Lytovchenko <dmytro.lytovchenko@gmail.com>
%%% Created : 19. Sep 2014
%%%-------------------------------------------------------------------
-module(tx_store).

%% API
-export([value/1]).
-export([start/0, store/1, read/1, list_json/0, delete/1]).

-define(ets_tab, tx_term_store).

-record(txvalue, {id, created, expires, value}).

value(#txvalue{value=V}) -> V.

start() ->
  ets:new(?ets_tab, [named_table, public, {keypos, #txvalue.id}]).

store(Term) ->
  {MegaS, S, MicroS} = os:timestamp(),
  Now = MegaS * 1000000 + S,
  Id = iolist_to_binary(io_lib:format("~p.~p", [Now, MicroS])),
  Expire = 3600 * 1000,
  TxValue = #txvalue{ id = Id
                    , created = Now
                    , expires = Now + Expire
                    , value = Term
                    },
  ets:insert(?ets_tab, TxValue),
  Id.

read(Id) ->
  [Value] = ets:lookup(?ets_tab, Id),
  Value.

list_json() ->
  Entries = list_keys(ets:first(?ets_tab), []),
  {struct, [{entries, {array, Entries}}]}.

list_keys('$end_of_table', A) -> A;
list_keys(K, A) ->
  [Value] = ets:lookup(?ets_tab, K),
  Entry =   {struct, [ {id, Value#txvalue.id}
                     , {created, Value#txvalue.created}
                     ]},
  list_keys(ets:next(?ets_tab, K), [Entry | A]).

delete(<<"all">>) ->
  ets:delete_all_objects(?ets_tab);
delete(Id) ->
  ets:delete(?ets_tab, Id).
