-module(step4_if_fn_do).

-export([main/0]).

-define(DEBUG, true).

read(Arg) ->
    reader:read_str(Arg).

eval_form({list, [{symbol, "def!"}|Args]}, ReplEnv) ->
    [{symbol, Name}| [Body| _Rest]] = Args,
    Evald = eval(Body, ReplEnv),
    {RetEnv, Val} =env:set(Name, Evald, ReplEnv),
    {RetEnv, Val};
eval_form(Ast, ReplEnv) -> {ReplEnv, eval(Ast, ReplEnv)}.

eval({list, Content}, ReplEnv) ->
    io:format("evaling list ~p~n", [Content]),
    [Func| Args] = Content,
    case Func of
        {symbol, "let*"} -> [{list, Bindings}, Body] = Args,
                            ReplEnv2 = let_bind(Bindings, ReplEnv),
                            Evald = eval(Body, ReplEnv2),
                            Evald;
        {symbol, "do"} -> lists:foldl(fun (Term, Res) ->
                                              eval(Term, ReplEnv)
                                      end, [], Args);
        {symbol, "if"} -> [Cond| [Clause| Else]] = Args,
                          Res = eval(Cond, ReplEnv),
                          case Res of
                              nil -> 
                                  case Else of
                                      [] -> {symbol, nil};
                                      [Final] -> eval(Final, ReplEnv)
                                  end;
                              false ->
                                  case Else of
                                      [] -> {symbol, nil};
                                      [Final] -> eval(Final, ReplEnv)
                                  end;
                              _ -> eval(Clause, ReplEnv)
                          end;
        {symbol, "fn*"} ->
            [{list, Fst}, Snd] = Args,
            fun(ReplEnvNew, ClosureArgs) ->

                              NewRepl = env:combine(ReplEnvNew, env:new(ReplEnv, Fst, ClosureArgs)),
                              io:format("go for the inside~p~n", [env:get_all_kv(NewRepl)]),
                              io:format("just inserted ~p to ~p~n", [Fst, ClosureArgs]),
                              io:format("new evaling ~p~n", [Snd]),
                              Res = eval(Snd, NewRepl),
                              Res
            end;

        _ ->
            {list, Content2} = eval_ast({list, Content}, ReplEnv),
            [F| Args2] = Content2,

            io:format("going for the apply ~p~n", [env:get_all_kv(ReplEnv)]),
            erlang:apply(F, [ReplEnv, Args2])
    end;

eval(Ast, ReplEnv) ->
    io:format("evaling ast ~p~n", [Ast]),
    io:format("and env is ~p~n", [env:get_all_kv(ReplEnv)]),
    eval_ast(Ast, ReplEnv).

eval_ast({list, Content}, ReplEnv) ->
    io:format("evalast list~n"),
    %Evald = lists:map(fun(Ast) ->
    %                          {ReplEnv2, Ev} = eval(Ast, ReplEnv),
    %                          Ev end, Content),
    Evald = lists:foldl(fun(Ast, Val) ->
                              Ev = eval(Ast, ReplEnv),
                              io:format("evaling list ~p~n", [Ev]),
                              Val++[Ev] end, [], Content),
    {list, Evald};
eval_ast({symbol, Symbol}, ReplEnv) ->
    io:format("evalast symbol~n"),
    io:format("Env is~p~n", [env:get_all_kv(ReplEnv)]),
    env:get(Symbol, ReplEnv);
eval_ast(Ast, _ReplEnv) ->
    io:format("evalast ast ~p~n", [Ast]),
    Ast.

let_bind([], ReplEnv) -> 
    ReplEnv;
let_bind([{symbol, A}|[B|Args]], Outer) ->
    ReplEnv = env:new(Outer),
    Evald = eval(B, ReplEnv),
    {ReplEnv3, Evald} = env:set(A, Evald, ReplEnv),
    let_bind(Args, ReplEnv3).

print(Ast) ->
    printer:pr_str(Ast, true).

main() -> main(
            env:newRepl()
           ).
main(ReplEnv) ->
    %ReplEnv2 = rep(ReplEnv, "(def! not (fn* (a) (if a false true)))"),
    main_loop(ReplEnv).

main_loop(ReplEnv) ->
    case io:get_line("mal-user> ") of
        eof -> ok;
        Line -> CleanLine = re:replace(Line, "(^\\s+)|(\\s+$)", "", [global,{return,list}]),
                case ?DEBUG of
                    true -> 
                        ReplEnv2 = rep_debug(ReplEnv, CleanLine),
                        main_loop(ReplEnv2);
                    false -> 
                        ReplEnv2 = rep(ReplEnv, CleanLine),
                        main_loop(ReplEnv2)
                end
    end.

rep_debug(ReplEnv, Line) ->
                Read = read(Line),
                {ReplEnv2, Evald} = eval_form(Read, ReplEnv),
                io:format("~s~n", [print(Evald)]),

                io:format("after the line~p~n", [env:get_all_kv(ReplEnv2)]),
                ReplEnv2.

rep(ReplEnv, Line) ->
    try eval_form(read(Line), ReplEnv) of
        {ReplEnv2, Evald} -> io:format("~s~n", [print(Evald)]),
                             ReplEnv2
    catch _:Error -> io:format("~p~n", [Error])
    end.
