-module(printer).

-export([pr_str/1]).

pr_str([]) ->
    ok;
pr_str(MalStructure) ->
    {Type, Content} = MalStructure,
    case Type of
        symbol -> Content;
        number -> Content;
        list -> ["("| [string:join(lists:map(fun pr_str/1, Content), " ")|")"]];
        quote -> ["(quote "| [string:join(lists:map(fun pr_str/1, Content), " ")|")"]];
        quasiquote -> ["(quasiquote "| [string:join(lists:map(fun pr_str/1, Content), " ")|")"]];
        unquote -> ["(unquote "| [string:join(lists:map(fun pr_str/1, Content), " ")|")"]];
        splice_unquote -> ["(splice-unquote "| [string:join(lists:map(fun pr_str/1, Content), " ")|")"]]
    end.
