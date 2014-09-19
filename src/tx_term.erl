%%%-------------------------------------------------------------------
%%% @doc Format any term to JSON
%%% @end
%%% @author Dmytro Lytovchenko <dmytro.lytovchenko@gmail.com>
%%% Created : 19. Sep 2014
%%%-------------------------------------------------------------------
-module(tx_term).

%% API
-export([to_json/1]).

-define(list_id,      l).
-define(tuple_id,     t).
-define(float_id,     f).
-define(integer_id,   i).
-define(atom_id,      a).
-define(binary_id,    b).
-define(bitstring_id, bs).
-define(pid_id,       p).
-define(ref_id,       r).
-define(fun_id,       'fun').
-define(port_id,      port).
-define(map_id,       m).
-define(unknown_id,   unknown).

%% @doc Formats term as JSON. Each value is represented by JSON dictionary
%% (hash) with type stored in 't' (values: l=list, t=tuple, f=float,
%% a=atom, i=integer, bs=bitstring, b=binary, pid, ref, fun, port and
%% unrecognized_type), and value stored as string or list where appropriate in v
to_json(Term) when is_list(Term) ->
  {struct, [ {t, ?list_id}
  , {v, {array, [to_json(Value) || Value <- Term]}}
  ]};
to_json(Term) when is_tuple(Term) ->
  {struct, [ {t, ?tuple_id}
  , {v, {array, [to_json(Value) || Value <- tuple_to_list(Term)]}}
  ]};

to_json(Term) when is_float(Term) ->
  {struct, [ {t, ?float_id}
  , {v, erl_print(Term)}
  ]};
to_json(Term) when is_atom(Term) ->
  {struct, [ {t, ?atom_id}
  , {v, atom_to_binary(Term, utf8)}
  ]};
to_json(Term) when is_integer(Term) ->
  {struct, [ {t, ?integer_id}
  , {v, list_to_binary(integer_to_list(Term))}
  ]};
to_json(Term) when is_binary(Term) ->
  {struct, [ {t, ?binary_id}
           , {v, erl_print(Term)}
           ]};
to_json(Term) when is_bitstring(Term) ->
  {struct, [ {t, ?bitstring_id}
  , {v, erl_print(Term)}
  ]};

to_json(Term) when is_pid(Term) ->
  {struct, [ {t, ?pid_id}
  , {v, erl_print(Term)}
  ]};
to_json(Term) when is_reference(Term) ->
  {struct, [ {t, ?ref_id}
  , {v, erl_print(Term)}
  ]};
to_json(Term) when is_function(Term) ->
  Info = erlang:fun_info(Term),
  {struct, [ {t, ?fun_id}
  , {n, proplists:get_value(name, Info)}
  , {a, proplists:get_value(arity, Info)}
  , {m, proplists:get_value(module, Info)}
  ]};
to_json(Term) when is_port(Term) ->
  {struct, [ {t, ?port_id}
  , {v, erl_print(Term)}
  ]};

to_json(Term) ->
  {struct, [ {t, ?unknown_id}
  , {v, erl_print(Term)}
  ]}.

erl_print(T) -> iolist_to_binary(io_lib:format("~p", [T])).