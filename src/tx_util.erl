%%%-------------------------------------------------------------------
%%% @doc Utilities + borrowed stuff where marked
%%% @end
%%% @author Dmytro Lytovchenko <dmytro.lytovchenko@gmail.com>
%%% Created : 21. Sep 2014
%%%-------------------------------------------------------------------
-module(tx_util).

%% API
-export([unquote/1]).

%% @private
%% @doc Unquote a URL encoded string.
%% File: mochiweb_util.erl
%% author: Bob Ippolito <bob@mochimedia.com>
%% copyright: 2007 Mochi Media, Inc.
-spec unquote(string() | binary()) -> string().
unquote(Binary) when is_binary(Binary) ->
  unquote(binary_to_list(Binary));
unquote(String) ->
  qs_revdecode(lists:reverse(String)).

%% @private
%% File: mochiweb_util.erl
%% author: Bob Ippolito <bob@mochimedia.com>
%% copyright: 2007 Mochi Media, Inc.
qs_revdecode(S) -> qs_revdecode(S, []).

-define(IS_HEX(C), ((C >= $0 andalso C =< $9) orelse
  (C >= $a andalso C =< $f) orelse
  (C >= $A andalso C =< $F))).

%% @private
%% File: mochiweb_util.erl
%% author: Bob Ippolito <bob@mochimedia.com>
%% copyright: 2007 Mochi Media, Inc.
qs_revdecode([], Acc) ->
  Acc;
qs_revdecode([$+ | Rest], Acc) ->
  qs_revdecode(Rest, [$\s | Acc]);
qs_revdecode([Lo, Hi, $% | Rest], Acc) when ?IS_HEX(Lo), ?IS_HEX(Hi) ->
  qs_revdecode(Rest, [(unhexdigit(Lo) bor (unhexdigit(Hi) bsl 4)) | Acc]);
qs_revdecode([C | Rest], Acc) ->
  qs_revdecode(Rest, [C | Acc]).

%% @private
%% File: mochiweb_util.erl
%% author: Bob Ippolito <bob@mochimedia.com>
%% copyright: 2007 Mochi Media, Inc.
unhexdigit(C) when C >= $0, C =< $9 -> C - $0;
unhexdigit(C) when C >= $a, C =< $f -> C - $a + 10;
unhexdigit(C) when C >= $A, C =< $F -> C - $A + 10.
