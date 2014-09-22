%%%-------------------------------------------------------------------
%%% @doc Erlang Server Interface (ESI) module doing content generation
%%% @end
%%% @author Dmytro Lytovchenko <dmytro.lytovchenko@gmail.com>
%%% Created : 19. Sep 2014
%%%-------------------------------------------------------------------
-module(tx_esi).

%% API
-export([show/3, list/3, delete/3]).

%% @doc Given stored=ID reads from ETS and encodes term to JSON;
%% Given inspect=pickle (base64 encoded term_to_binary) decodes it and attempts
%% to provide information about this object. Like process_info or port_info.
show(Sid, Env, In) -> try_do(fun do_show/3, Sid, Env, In).
do_show(Sid, Env, _In) ->
  QueryString = proplists:get_value(query_string, Env, ""),
  Query       = parse_query_string(QueryString),
  RespJson = case proplists:get_value("stored", Query) of
    undefined ->
      case proplists:get_value("inspect", Query) of
        undefined -> [{error, "query must contain either stored=ID or "
                              "inspect=base64pickle"}];
        PickleStr ->
          Pickle  = list_to_binary(PickleStr),
          Subject = binary_to_term(base64:decode(Pickle), [safe]),
          InspectResult = tx_term:inspect(Subject),
          RawString = iolist_to_binary(io_lib:format("~120p.", [InspectResult])),
          {struct, [ {parsed, tx_term:to_json(InspectResult)}
                   , {raw, RawString}
                   ]}
      end;
    Id ->
      StoredTuple = tx_store:read(list_to_binary(Id)),
      Value       = tx_store:value(StoredTuple),
      RawString   = iolist_to_binary(io_lib:format("~120p.", [Value])),
      {struct, [ {parsed, tx_term:to_json(Value)}
               , {raw, RawString}
               , {title, tx_store:title(StoredTuple)}
               , {expires, tx_store:expires(StoredTuple)}
      ]}
  end,
  mod_esi:deliver(Sid, tx_mochijson2:encode(RespJson)).

delete(Sid, Env, In) -> try_do(fun do_delete/3, Sid, Env, In).
do_delete(Sid, Env, _In) ->
  Id = list_to_binary(proplists:get_value(query_string, Env)),
  tx_store:delete(Id),
  redirect(Sid, Env).

list(Sid, Env, In) -> try_do(fun do_list/3, Sid, Env, In).
do_list(Sid, _Env, _In) ->
  Json = tx_store:list_json(),
  Resp = tx_mochijson2:encode(Json),
  mod_esi:deliver(Sid, Resp).

%%==============================================================================
%% @private
redirect(Sid, Env) ->
  H = proplists:get_value(http_host, Env),
  mod_esi:deliver(Sid, "status: 302 Found\r\n"
  "Location: http://" ++ H ++ "/index.html\r\n"
  "\r\n"),
  Body = io_lib:format("<html>"
  "<script>window.location='/index.html';</script>"
  "</html>", []),
  mod_esi:deliver(Sid, Body).

%% @private
try_do(F, Sid, Env, In) ->
  try
    F(Sid, Env, In)
  catch T:E ->
    Stack = erlang:get_stacktrace(),
    Json = tx_term:to_json([{error, "exception in tx web handler"},
                            {what, {T, E}},
                            {stack, Stack}
                           ]),
    io:format("~p~n", [{T, E, Stack}]),
    Resp = tx_mochijson2:encode(Json),
    mod_esi:deliver(Sid, Resp)
  end.

%% @private
%% @doc Splits a=b&c=d into [{"a", "b"}, {"c", "d"}], leaves URL encoding intact
parse_query_string(Str) ->
  Pairs = string:tokens(Str, "&"),
  KeyValues0 = lists:map(fun(Pair) -> string:tokens(Pair, "=") end, Pairs),
  KeyValues1 = lists:map(fun erlang:list_to_tuple/1, KeyValues0),
  [{Key, tx_util:unquote(Value)} || {Key, Value} <- KeyValues1].
