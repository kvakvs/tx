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
show(Sid, Env, _In) ->
  QueryString = proplists:get_value(query_string, Env, ""),
  Query       = parse_query_string(QueryString),
  Json = case proplists:get_value("stored", Query) of
    undefined ->
      case proplists:get_value("inspect", Query) of
        undefined -> [{error, <<"query must contain either stored=ID or ",
                                "inspect=base64pickle">>}];
        PickleStr ->
          Pickle  = list_to_binary(PickleStr),
          Subject = binary_to_term(base64:decode(Pickle), [safe]),
          tx_term:inspect_as_json(Subject)
      end;
    Id ->
      StoredValue = tx_store:read(list_to_binary(Id)),
      tx_term:to_json(tx_store:value(StoredValue))
  end,
  Resp = tx_mochijson2:encode(Json),
  mod_esi:deliver(Sid, Resp).

delete(Sid, Env, _In) ->
  Id = list_to_binary(proplists:get_value(query_string, Env)),
  tx_store:delete(Id),
  redirect(Sid, Env).

redirect(Sid, Env) ->
  H = proplists:get_value(http_host, Env),
  mod_esi:deliver(Sid, "status: 302 Found\r\n"
                       "Location: http://" ++ H ++ "/index.html\r\n"
                       "\r\n"),
  Body = io_lib:format("<html>"
                       "<script>window.location='/index.html';</script>"
                       "</html>", []),
  mod_esi:deliver(Sid, Body).

list(Sid, _Env, _In) ->
  Json = tx_store:list_json(),
  Resp = tx_mochijson2:encode(Json),
  mod_esi:deliver(Sid, Resp).

%% @doc Splits a=b&c=d into [{"a", "b"}, {"c", "d"}], leaves URL encoding intact
parse_query_string(Str) ->
  Pairs = string:tokens(Str, "&"),
  KeyValues = lists:map(fun(Pair) -> string:tokens(Pair, "=") end, Pairs),
  lists:map(fun erlang:list_to_tuple/1, KeyValues).
