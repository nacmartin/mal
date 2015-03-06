-module(core).

-export([core_ns/0]).

core_ns() ->
    [{"list", fun (Env, Args) -> {list, Args} end},
     {"list?", fun (Env, [Arg]) -> case Arg of
                                  {list, _} -> true;
                                  _ -> false
                              end end},
     {"empty?", fun (Env, Arg) -> 
                        case Arg of
                                 [{list, []}] -> true;
                                 _ -> false
                             end end},
     {"count", fun (Env, Arg) -> 
                       case Arg of
                           [{list, Body}] -> length(Body);
                           [nil] -> 0;
                           _SMT -> erlang:error("count applied to not a list")
                       end end},
     {"prn-str", fun (Env, Args) ->
                         PrStr = printer:pr_str(true),
                         string:join(lists:map(PrStr, Args), " ")
                 end},
     {"prn", fun (Env, Args) ->
                     PrStr = printer:pr_str(false),
                     io:format("~s~n", [string:join(lists:map(PrStr, Args), " ")]),
                     nil
             end},
     {"prntln", fun (Env, Args) ->
                         PrStr = printer:pr_str(false),
                         string:join(lists:map(PrStr, Args), " ")
                 end},
     {"prn", fun (Env, Args) ->
                     PrStr = printer:pr_str(true),
                     io:format("~s~n", [string:join(lists:map(PrStr, Args), " ")]),
                     nil
             end},
     {">", fun (Env, [A, B]) -> case A > B of
                               true -> true;
                               false -> false
                           end end},
     {">=", fun (Env, [A, B]) -> case A >= B of
                               true -> true;
                               false -> false
                           end end},
     {"<", fun (Env, [A, B]) -> case A < B of
                               true -> true;
                               false -> false
                           end end},
     {"<=", fun (Env, [A, B]) -> case A =< B of
                               true -> true;
                               false -> false
                           end end},
     {"=", fun (Env, [A, B]) -> case A =:= B of
                               true -> true;
                               false -> false
                           end end},
     {"+", fun (Env, [A, B]) -> 
                   A + B end},
     {"-", fun (Env, [A, B]) -> 
                   A - B end},
     {"*", fun (Env, [A, B]) -> A * B end},
     {"/", fun (Env, [A, B]) -> erlang:trunc(A / B) end}].
