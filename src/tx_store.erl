%%%-------------------------------------------------------------------
%%% @doc Stores terms in ETS table. Cleans records eventually
%%% @end
%%% @author Dmytro Lytovchenko <dmytro.lytovchenko@gmail.com>
%%% Created : 19. Sep 2014
%%%-------------------------------------------------------------------
-module(tx_store).

%% API
-export([value/1, title/1, expires/1]).
-export([start/0, store/2, read/1, list_json/0, delete/1]).

-define(ets_tab, tx_term_store).

-record(txvalue, {id, created, expires, value, title}).

value(#txvalue{value=V}) -> V.
title(#txvalue{title=V}) -> V.
expires(#txvalue{expires=V}) -> V.

start() ->
  ets:new(?ets_tab, [named_table, public, {keypos, #txvalue.id}]).

store(Term, Title) ->
  {MegaS, S, MicroS} = os:timestamp(),
  Now = MegaS * 1000000 + S,
  Id = iolist_to_binary(io_lib:format("~p.~p", [Now, MicroS])),
  Expire = 3600 * 1000,
  TxValue = #txvalue{ id      = Id
                    , created = Now
                    , expires = Now + Expire
                    , value   = Term
                    , title   = tx_util:as_binary(Title)
                    },
  ets:insert(?ets_tab, TxValue),
  Id.

read(Id) ->
  [Value] = ets:lookup(?ets_tab, Id),
  Value.

list_json() ->
  Entries = list_objects(ets:first(?ets_tab), []),
  {struct, [{entries, {array, Entries}}]}.

list_objects('$end_of_table', A) -> A;
list_objects(K, A) ->
  [Value] = ets:lookup(?ets_tab, K),
  Entry =   {struct, [ {id,      Value#txvalue.id}
                     , {created, Value#txvalue.created}
                     , {expires, Value#txvalue.expires}
                     , {title,   Value#txvalue.title}
                     ]},
  list_objects(ets:next(?ets_tab, K), [Entry | A]).

delete(<<"all">>) ->
  ets:delete_all_objects(?ets_tab);
delete(Id) ->
  ets:delete(?ets_tab, Id).
