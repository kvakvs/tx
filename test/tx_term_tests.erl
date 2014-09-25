%%%-------------------------------------------------------------------
%%% @doc See that some corner cases are covered
%%% @end
%%% Created : 25. Sep 2014 12:20
%%%-------------------------------------------------------------------
-module(tx_term_tests).

%% API
-export([utf8_bad_term_test/0]).

utf8_bad_term_test() ->
  X = {error,{asn1,{wrong_tag,{{expected,16},
    {got,131073,
      {131073,
        [{2,<<0>>},
          {2,<<0>>},
          {16,
            [{131072,<<1>>},
              {131074,<<4,16,151,32,130,4,6,240>>},
              {131075,<<132,17,151,37,132,146,8,7>>},
              {131077,<<"\n">>},
              {131080,<<0>>},
              {131082,<<132,147,24,38,3>>},
              {131084,<<3,20,89,66,40,137,80>>},
              {131095,<<145,129>>},
              {131099,[{131072,<<128,144,163>>}]},
              {131100,<<2>>},
              {131101,<<3,20,89,66,40,137,80>>},
              {131102,<<3,97>>},
              {131122,<<82,32,80,5,96,41,152,249>>},
              {131125,[{131075,<<17>>}]},
              {131126,<<65,51,1,54,3,55,150>>},
              {131127,<<145,151,32,130,147,149,241>>},
              {131129,<<2,65,144,48,97,16,101,97>>},
              {131131,
                [{131073,
                  <<145,151,32,130,147,149,241>>}]}]}]}}}}}},
%%   io:format(user, "~p.", [tx_term:to_json(X)]).
  tx:show(X).