-module(core).

-export([core_ns/0]).

core_ns() ->
    [{"list", fun (Env, Args) -> {Env, {list, Args}} end},
     {"list?", fun (Env, [Arg]) -> {Env, case Arg of
                                  {list, _} -> true;
                                  _ -> false
                                         end} end},
     {"empty?", fun (Env, Arg) -> 
                        {Env, case Arg of
                                 [{list, []}] -> true;
                                 _ -> false
                              end} end},
     {"count", fun (Env, Arg) -> 
                       {Env, case Arg of
                           [{list, Body}] -> length(Body);
                           [nil] -> 0;
                           _SMT -> erlang:error("count applied to not a list")
                             end} end},
     {"pr-str", fun (Env, Args) ->
                         PrStr = printer:pr_str(true),
                         {Env, string:join(lists:map(PrStr, Args), " ")}
                 end},
     {"str", fun (Env, Args) ->
                     PrStr = printer:pr_str(false),
                     io:format("~s~n", [string:join(lists:map(PrStr, Args), " ")]),
                     {Env, nil}
             end},
     {"println", fun (Env, Args) ->
                         PrStr = printer:pr_str(false),
                         {Env, string:join(lists:map(PrStr, Args), " ")}
                 end},
     {"prn", fun (Env, Args) ->
                     PrStr = printer:pr_str(true),
                     io:format("~s~n", [string:join(lists:map(PrStr, Args), " ")]),
                     {Env, nil}
             end},
     {">", fun (Env, [A, B]) -> {Env, case A > B of
                               true -> true;
                               false -> false
                                      end} end},
     {">=", fun (Env, [A, B]) -> {Env, case A >= B of
                               true -> true;
                               false -> false
                                       end} end},
     {"<", fun (Env, [A, B]) -> {Env, case A < B of
                               true -> true;
                               false -> false
                                end} end},
     {"<=", fun (Env, [A, B]) -> {Env, case A =< B of
                               true -> true;
                               false -> false
                                       end} end},
     {"=", fun (Env, [A, B]) -> {Env, case A =:= B of
                               true -> true;
                               false -> false
                                      end} end},
     {"+", fun (Env, [A, B]) -> 
                   {Env, A + B} end},
     {"-", fun (Env, [A, B]) -> 
                   {Env, A - B} end},
     {"*", fun (Env, [A, B]) -> {Env, A * B} end},
     {"/", fun (Env, [A, B]) -> {Env, erlang:trunc(A / B)} end}].
