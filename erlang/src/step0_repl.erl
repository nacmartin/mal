-module(step0_repl).

-export([main/0]).

read(Arg) ->
    Arg.

eval(Arg) ->
    Arg.

print(Arg) ->
    Arg.

main() ->
    case io:get_line("") of
        eof -> ok;
        Line -> io:format(print(eval(read(Line)))),
                main()
    end.
