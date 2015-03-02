-module(step2_eval).

-export([main/0]).

read(Arg) ->
    reader:read_str(Arg).

eval({list, Content}, ReplEnv) ->
    {list, Content2} = eval_ast({list, Content}, ReplEnv),
    [Func| Args] = Content2,
    F = find_func(Func, ReplEnv),
    erlang:apply(F, Args);

eval(Ast, ReplEnv) ->
    eval_ast(Ast, ReplEnv).

find_func(Symbol, []) ->
    erlang:error("'"++Symbol++"' not found");
find_func(Func, [EnvEntry|ReplEnv]) ->
    {Symbol, Fun} = EnvEntry,
    case Symbol of
        Func -> Fun;
        _ -> find_func(Func, ReplEnv)
    end.


eval_ast({list, Content}, ReplEnv) ->
    {list, lists:map(fun(Ast)-> eval(Ast, ReplEnv) end, Content)};
eval_ast({symbol, Symbol}, []) ->
    erlang:error("'"++Symbol++"' not found");
eval_ast({symbol, Content}, [{Symbol, _Func}|ReplEnv]) ->
    case Symbol of
        Content -> Content;
        _ -> eval_ast({symbol, Content}, ReplEnv)
    end;
eval_ast(Ast, _ReplEnv) ->
    Ast.


print(Ast) ->
    printer:pr_str(Ast).

main() ->
    ReplEnv = [{"+", fun (A, B) -> A + B end},
               {"-", fun (A, B) -> A - B end},
               {"*", fun (A, B) -> A * B end},
               {"/", fun (A, B) -> erlang:trunc(A / B) end}
              ],
    case io:get_line("mal-user> ") of
        eof -> ok;
        Line -> CleanLine = re:replace(Line, "(^\\s+)|(\\s+$)", "", [global,{return,list}]),
                %Read = read(CleanLine),
                %io:format("~s~n", [print(eval(Read, ReplEnv))]),
                try eval(read(CleanLine), ReplEnv) of
                    Evald -> io:format("~s~n", [print(Evald)])
                catch _:Error -> io:format("~p~n", [Error])
                end,
                main()
    end.
