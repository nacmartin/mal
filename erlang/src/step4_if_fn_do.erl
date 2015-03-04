-module(step4_if_fn_do).

-export([main/0]).

read(Arg) ->
    reader:read_str(Arg).

eval({list, Content}, ReplEnv) ->
    [Func| Args] = Content,
    case Func of
        {symbol, "def!"} -> [{symbol, Name}| [Body| _Rest]] = Args,
                            {ReplEnv2, Evald} = eval(Body, ReplEnv),
                            env:set(Name, Evald, ReplEnv2);
        {symbol, "let*"} -> [{list, Bindings}, Body] = Args,
                            ReplEnv2 = let_bind(Bindings, ReplEnv),
                            {_ReplEnv3, Evald} = eval(Body, ReplEnv2),
                            {ReplEnv, Evald};
        {symbol, "if"} -> [Cond| [Clause| Else]] = Args,
                          {ReplEnv2, Res} = eval(Cond, ReplEnv),
                           case Res of
                               "nil" -> 
                                   case Else of
                                       [] -> {ReplEnv2, {symbol, "nil"}};
                                       [Final] -> eval(Final, ReplEnv2)
                                   end;
                               "false" ->
                                   case Else of
                                       [] -> {ReplEnv2, {symbol, "nil"}};
                                       [Final] -> eval(Final, ReplEnv2)
                                   end;
                               _ -> eval(Clause, ReplEnv2)
                           end;
        {symbol, "fn*"} ->
            [{list, Fst}, Snd] = Args,
            {ReplEnv, {function, fun(RE, ClosureArgs) ->
                    NewRepl = env:new(RE, Fst, ClosureArgs),
                    {_, Res} = eval(Snd, NewRepl),
                    Res
            end}};

        _ ->
            {ReplEnv2, {list, Content2}} = eval_ast({list, Content}, ReplEnv),
            [F| Args2] = Content2,

            case F of
                {function, F2} -> 
                    {ReplEnv2, erlang:apply(F2, [ReplEnv| [Args2]])};
                _ -> {ReplEnv2, erlang:apply(F, [Args2])}
            end
    end;

eval(Ast, ReplEnv) ->
    {ReplEnv, eval_ast(Ast, ReplEnv)}.

eval_ast({list, Content}, ReplEnv) ->
    {ReplEnv4, Evald2} = lists:foldl(fun(Ast, {ReplEnv2, Evald}) ->
                        {ReplEnv3, Ev} = eval(Ast, ReplEnv2),
                        {ReplEnv3, Evald ++ [Ev]}
                end
                , {ReplEnv, []}, Content),
    {ReplEnv4, {list, Evald2}};
eval_ast({symbol, Symbol}, ReplEnv) ->
    case env:find(Symbol, ReplEnv) of
        nil -> Symbol;
        Value -> Value
    end;
eval_ast(Ast, _ReplEnv) ->
    Ast.

let_bind([], ReplEnv) -> 
    ReplEnv;
let_bind([{symbol, A}|[B|Args]], Outer) ->
    ReplEnv = env:new(Outer),
    {ReplEnv2, Evald} = eval(B, ReplEnv),
    {ReplEnv3, Evald} = env:set(A, Evald, ReplEnv2),
    let_bind(Args, ReplEnv3).

print(Ast) ->
    printer:pr_str(Ast).

main() -> main(
            env:newRepl()
           ).
main(ReplEnv) ->
    case io:get_line("mal-user> ") of
        eof -> ok;
        Line -> CleanLine = re:replace(Line, "(^\\s+)|(\\s+$)", "", [global,{return,list}]),
                Read = read(CleanLine),
                {ReplEnv2, Evald} = eval(Read, ReplEnv),
                io:format("~s~n", [print(Evald)]),
                main(ReplEnv2)
                %try eval(read(CleanLine), ReplEnv) of
                %    {ReplEnv2, Evald} -> io:format("~s~n", [print(Evald)]),
                %                         main(ReplEnv2)
                %catch _:Error -> io:format("~p~n", [Error])
                %end
    end.
