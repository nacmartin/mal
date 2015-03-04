-module(core).

-export([core_ns/0]).

core_ns() ->
    [{"list", fun (Args) -> {list, Args} end},
     {"list?", fun ([Arg]) -> case Arg of
                                  {list, _} -> true;
                                  _ -> false
                              end end},
     {"empty?", fun (Arg) -> 
                        case Arg of
                                 [{list, []}] -> true;
                                 _ -> false
                             end end},
     {"count", fun (Arg) -> 
                        case Arg of
                                 [{list, Body}] -> length(Body);
                                 ["nil"] -> 0;
                                 _ -> erlang:error("count applied to not a list")
                             end end},
     {">", fun ([A, B]) -> case A > B of
                               true -> "true";
                               false -> "false"
                           end end},
     {">=", fun ([A, B]) -> case A >= B of
                               true -> "true";
                               false -> "false"
                           end end},
     {"<", fun ([A, B]) -> case A < B of
                               true -> "true";
                               false -> "false"
                           end end},
     {"<=", fun ([A, B]) -> case A =< B of
                               true -> "true";
                               false -> "false"
                           end end},
     {"=", fun ([A, B]) -> case A =:= B of
                               true -> "true";
                               false -> "false"
                           end end},
     {"+", fun ([A, B]) -> A + B end},
     {"-", fun ([A, B]) -> A - B end},
     {"*", fun ([A, B]) -> A * B end},
     {"/", fun ([A, B]) -> erlang:trunc(A / B) end}].
