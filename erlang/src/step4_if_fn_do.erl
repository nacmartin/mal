-module(step4_if_fn_do).

-export([main/0]).

-define(DEBUG, true).

read(Arg) ->
    reader:read_str(Arg).

eval({list, Content}, ReplEnv) ->
    io:format("evaling list ~p~n", [Content]),
    [Func| Args] = Content,
    case Func of
        {symbol, "def!"} -> [{symbol, Name}| [Body| _Rest]] = Args,
                            {_ReplEnv2, Evald} = eval(Body, ReplEnv),
                            {RetEnv, Val} =env:set(Name, Evald, ReplEnv),
                            {RetEnv, Val};
        {symbol, "let*"} -> [{list, Bindings}, Body] = Args,
                            ReplEnv2 = let_bind(Bindings, ReplEnv),
                            {_, Evald} = eval(Body, ReplEnv2),
                            {ReplEnv, Evald};
        {symbol, "do"} -> lists:foldl(fun (Term, {ReplEnv2, Res}) ->
                                              eval(Term, ReplEnv2)
                                      end, {ReplEnv, []}, Args);
        {symbol, "if"} -> [Cond| [Clause| Else]] = Args,
                          {_ReplEnv2, Res} = eval(Cond, ReplEnv),
                           case Res of
                               nil -> 
                                   case Else of
                                       [] -> {ReplEnv, {symbol, nil}};
                                       [Final] -> eval(Final, ReplEnv)
                                   end;
                               false ->
                                   case Else of
                                       [] -> {ReplEnv, {symbol, nil}};
                                       [Final] -> eval(Final, ReplEnv)
                                   end;
                               _ -> eval(Clause, ReplEnv)
                           end;
        {symbol, "fn*"} ->
            [{list, Fst}, Snd] = Args,
            {ReplEnv, fun(ReplEnvNew, ClosureArgs) ->
                              NewRepl = env:new(ReplEnv, Fst, ClosureArgs),
                              io:format("go for the inside~p~n", [env:get_all_kv(NewRepl)]),
                              io:format("just inserted ~p to ~p~n", [Fst, ClosureArgs]),
                              io:format("new evaling ~p~n", [Snd]),
                              {RE3, Res} = eval(Snd, NewRepl),
                              io:format("going out with~p~n", [env:get_all_kv(ReplEnvNew)]),
                              {ReplEnvNew, Res}
            end};

        _ ->
            {RE, {list, Content2}} = eval_ast({list, Content}, ReplEnv),
            [F| Args2] = Content2,

            io:format("going for the apply ~p~n", [env:get_all_kv(RE)]),
            erlang:apply(F, [RE, Args2])
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
    {Env2, Evald} = lists:foldl(fun(Ast, {RE, Val}) ->
                              {ReplEnv2, Ev} = eval(Ast, RE),
                              {ReplEnv2, Val++[Ev]} end, {ReplEnv, []}, Content),
    {ReplEnv, {list, Evald}};
eval_ast({symbol, Symbol}, ReplEnv) ->
    io:format("evalast symbol~n"),
    io:format("Env is~p~n", [env:get_all_kv(ReplEnv)]),
    {ReplEnv, env:get(Symbol, ReplEnv)};
eval_ast(Ast, ReplEnv) ->
    io:format("evalast ast ~p~n", [Ast]),
    {ReplEnv, Ast}.

let_bind([], ReplEnv) -> 
    ReplEnv;
let_bind([{symbol, A}|[B|Args]], Outer) ->
    ReplEnv = env:new(Outer),
    {ReplEnv2, Evald} = eval(B, ReplEnv),
    {ReplEnv3, Evald} = env:set(A, Evald, ReplEnv2),
    let_bind(Args, ReplEnv3).

print(Ast) ->
    printer:pr_str(Ast, true).

main() -> main(
            env:newRepl()
           ).
main(ReplEnv) ->
    ReplEnv2 = rep(ReplEnv, "(def! not (fn* (a) (if a false true)))"),
    main_loop(ReplEnv2).

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
                {ReplEnv2, Evald} = eval(Read, ReplEnv),
                io:format("~s~n", [print(Evald)]),

                io:format("after the line~p~n", [env:get_all_kv(ReplEnv2)]),
                ReplEnv2.

rep(ReplEnv, Line) ->
    try eval(read(Line), ReplEnv) of
        {ReplEnv2, Evald} -> io:format("~s~n", [print(Evald)]),
                             ReplEnv2
    catch _:Error -> io:format("~p~n", [Error])
    end.
