-module(step4_if_fn_do).

-export([main/0]).

-define(DEBUG, true).

read(Arg) ->
    reader:read_str(Arg).

eval({list, [{symbol, "def!"}|Args]}, ReplEnv) ->
    [{symbol, Name}| [Body| _Rest]] = Args,
    {ReplEnv0, Evald} = eval(Body, ReplEnv),
    {RetEnv, Val} =env:set(Name, Evald, ReplEnv0),
    {RetEnv, Val};
%eval_form(Ast, ReplEnv) -> {ReplEnv, eval(Ast, ReplEnv)}.

eval({list, Content}, ReplEnv) ->
    [Func| Args] = Content,
    case Func of
        {symbol, "let*"} -> [{list, Bindings}, Body] = Args,
                            ReplEnv2 = let_bind(Bindings, ReplEnv),
                            Evald = eval(Body, ReplEnv2),
                            {ReplEnv, Evald};
        {symbol, "do"} -> lists:foldl(fun (Term, {Env, Res}) ->
                                              eval(Term, Env)
                                      end, {ReplEnv, []}, Args);
        {symbol, "if"} -> [Cond| [Clause| Else]] = Args,
                          {Env2, Res} = eval(Cond, ReplEnv),
                          case Res of
                              nil -> 
                                  case Else of
                                      [] -> {Env2, {symbol, nil}};
                                      [Final] -> eval(Final, ReplEnv)
                                  end;
                              false ->
                                  case Else of
                                      [] -> {Env2, {symbol, nil}};
                                      [Final] -> eval(Final, ReplEnv)
                                  end;
                              _ -> eval(Clause, Env2)
                          end;
        {symbol, "fn*"} ->
            [{list, Fst}, Snd] = Args,
            {ReplEnv, fun(ReplEnvDyn, ClosureArgs) ->

                              NewRepl = env:combine(ReplEnvDyn, env:new(ReplEnv, Fst, ClosureArgs)),
                              {_, Res} = eval(Snd, NewRepl),
                              {ReplEnvDyn, Res}
            end};

        _ ->
            {list, Content2} = eval_ast({list, Content}, ReplEnv),
            [F| Args2] = Content2,

            erlang:apply(F, [ReplEnv, Args2])
    end;

eval(Ast, ReplEnv) ->
    {ReplEnv, eval_ast(Ast, ReplEnv)}.

eval_ast({list, Content}, ReplEnv) ->
    %Evald = lists:map(fun(Ast) ->
    %                          {ReplEnv2, Ev} = eval(Ast, ReplEnv),
    %                          Ev end, Content),
    Evald = lists:foldl(fun(Ast, Val) ->
                                {Env, Ev} = eval(Ast, ReplEnv),
                              Val++[Ev] end, [], Content),
    {list, Evald};
eval_ast({symbol, Symbol}, ReplEnv) ->
    env:get(Symbol, ReplEnv);
eval_ast(Ast, _ReplEnv) ->
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

                ReplEnv2.

rep(ReplEnv, Line) ->
    try eval(read(Line), ReplEnv) of
        {ReplEnv2, Evald} -> io:format("~s~n", [print(Evald)]),
                             ReplEnv2
    catch _:Error -> io:format("~p~n", [Error])
    end.
