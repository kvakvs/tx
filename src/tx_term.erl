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
-define(string_id,    s).
-define(bitstring_id, bs).
-define(pid_id,       p).
-define(ref_id,       r).
-define(fun_id,       'fun').
-define(port_id,      port).
-define(map_id,       m).     % NYI
-define(unknown_id,   unknown).

%% @doc Formats term as JSON. Each value is represented by JSON dictionary
%% (hash) with type stored in 't' (values: l=list, t=tuple, f=float,
%% a=atom, i=integer, bs=bitstring, b=binary, pid, ref, fun, port and
%% unrecognized_type), and value stored as string or list where appropriate in v
to_json(Term) when is_list(Term) ->
  case is_printable(Term) of
    false ->
      {struct, [ {t, ?list_id}
               , {v, {array, [to_json(Value) || Value <- Term]}}
      ]};
    true ->
      {struct, [ {t, ?string_id}
               , {v, list_to_binary(Term)}
      ]}
  end;
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
           , {v, erl_print_binary(Term)}
           ]};
to_json(Term) when is_bitstring(Term) ->
  {struct, [ {t, ?bitstring_id}
  , {v, strip_angle_brackets(erl_print(Term))}
  ]};

to_json(Term) when is_pid(Term) ->
  {struct, [ {t, ?pid_id}
  , {v, erl_print(Term)}
  , {pickle, base64:encode(term_to_binary(Term))}
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
  , {info, to_json(erlang:port_info(Term))}
  , {pickle, base64:encode(term_to_binary(Term))}
  ]};

to_json(Term) ->
  {struct, [ {t, ?unknown_id}
  , {v, erl_print(Term)}
  ]}.

erl_print(T) -> iolist_to_binary(io_lib:format("~p", [T])).

erl_print_binary(T) ->
  case is_printable(binary_to_list(T)) of
    true  -> T;
    false -> strip_angle_brackets(erl_print(T))
%%     false -> list_to_binary(format_binary(T, []))
  end.

strip_angle_brackets(<<"<<", _/binary>> = Tail) ->
  binary:part(Tail, 2, byte_size(Tail)-4).

%% format_binary(<<>>, Accum) -> lists:reverse(Accum);
%% format_binary(<<Byte:8, Rest/binary>>, Accum0) ->
%%   Accum = case Accum0 of % insert comma if Accum not empty
%%             [] -> Accum0;
%%             _  -> [<<",">>, Accum0]
%%           end,
%%   Txt = integer_to_list(Byte),
%%   format_binary(Rest, [Txt | Accum]).

is_printable([]) -> true;
is_printable([X | _T]) when not is_integer(X) -> false;
is_printable([X | _T])
  when X =/= 9 andalso X =/= 10 andalso X =/= 13
    andalso (X < 32 orelse X > 255) -> false;
is_printable([_ | T]) -> is_printable(T).
