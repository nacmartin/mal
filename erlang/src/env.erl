-module(env).

-export([new/3, set/3, get/2, find/2, newRepl/0, get_all/1, get_all_kv/1, combine/2]).

new(Outer, Binds, Exprs) ->
    {Outer, build_bind_exprs(Binds, Exprs, dict:new())}.

combine(Father, {nil, C2}) -> {Father, C2};
combine(Father, {C1, C2}) ->
    {combine(Father, C1), C2}.


build_bind_exprs([], [], Env) -> Env;
build_bind_exprs([{symbol, "&"}|[Bind]], Exprs, Env) -> bind_variadic(Bind, Exprs, Env);
build_bind_exprs([{symbol, Bind}|Binds], [Expr|Exprs], Env) ->
    build_bind_exprs(Binds, Exprs, dict:store(Bind, Expr, Env)).

bind_variadic({symbol, Bind}, Exprs, Env) ->
    dict:store(Bind, {list, Exprs}, Env).

set(Key, Value, {Outer, Env}) ->
    {{Outer, dict:store(Key, Value, Env)}, Value}.

get_all_kv({nil, Env}) ->
    dict:to_list(Env);
get_all_kv({Outer, Env}) ->
    get_all_kv(Outer) ++ dict:to_list(Env).

get_all({nil, Env}) ->
    dict:fetch_keys(Env);
get_all({Outer, Env}) ->
    get_all(Outer) ++ dict:fetch_keys(Env).

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
