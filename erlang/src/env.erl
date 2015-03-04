-module(env).

-export([new/3, set/3, get/2, find/2, newRepl/0]).

new(Outer, Binds, Exprs) ->
    BindExpr = lists:zip(Binds, Exprs),
    {Outer ,lists:foldl(fun({{symbol, Binding}, Expression}, Dict) -> dict:store(Binding, Expression, Dict) end, dict:new(), BindExpr)}.

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
    ReplEnv = core:core_ns(),
    lists:foldl(fun({Symbol, Fun}, Env) -> 
                        {Repl, _Val} = env:set(Symbol, Fun, Env),
                        Repl
                end, env:new(nil, [], []), ReplEnv).
