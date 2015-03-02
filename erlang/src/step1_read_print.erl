-module(step1_read_print).

-export([main/0]).

read(Arg) ->
    reader:read_str(Arg).

eval(Arg) ->
    Arg.

print(Arg) ->
    printer:pr_str(Arg).

main() ->
    case io:get_line("mal-user> ") of
        eof -> ok;
        Line -> CleanLine = re:replace(Line, "(^\\s+)|(\\s+$)", "", [global,{return,list}]),
                %Read = read(CleanLine),
                %io:format("~s~n", [print(eval(Read))]),
                try read(CleanLine) of
                    Read -> io:format("~s~n", [print(eval(Read))])
                catch _:Error -> io:format("~p~n", [Error])
                end,
                main()
    end.
