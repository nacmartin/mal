-module(env).

-export([new/1, set/3, get/2, find/2, newRepl/0]).

new(Outer) ->
    {Outer, dict:new()}.

set(Key, Value, {Outer, Env}) ->
    {{Outer, dict:store(Key, Value, Env)}, Value}.

find(Key, {Outer, Env}) ->
    case dict:find(Key, Env) of
        error -> case Outer of
                     nil -> nil;
                     {Outer2, Env2} -> find(Key, {Outer2, Env2})
                 end;
        {ok, Value} -> Value
    end.

get(Key, {Outer, Env}) ->
    case find(Key, {Outer, Env}) of
        nil -> erlang:error("'"++Key++"' not found");
        Value -> Value
    end.

newRepl() ->
    ReplEnv = [{"+", fun (A, B) -> A + B end},
               {"-", fun (A, B) -> A - B end},
               {"*", fun (A, B) -> A * B end},
               {"/", fun (A, B) -> erlang:trunc(A / B) end}
              ],
    lists:foldl(fun({Symbol, Fun}, Env) -> 
                        {Repl, _Val} = env:set(Symbol, Fun, Env),
                        Repl
                end, env:new(nil), ReplEnv).

