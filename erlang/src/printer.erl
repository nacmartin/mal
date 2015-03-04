-module(printer).

-export([pr_str/1]).

pr_str([]) ->
    ok;
pr_str({Type, Content}) ->
    case Type of
        symbol -> Content;
        nil -> "";
        function -> "*";
        list -> ["("| [string:join(lists:map(fun pr_str/1, Content), " ")|")"]];
        vector -> ["["| [string:join(lists:map(fun pr_str/1, Content), " ")|"]"]];
        hashmap -> ["{"| [string:join(lists:map(fun pr_str/1, Content), " ")|"}"]];
        quote -> ["(quote "| [string:join(lists:map(fun pr_str/1, Content), " ")|")"]];
        quasiquote -> ["(quasiquote "| [string:join(lists:map(fun pr_str/1, Content), " ")|")"]];
        unquote -> ["(unquote "| [string:join(lists:map(fun pr_str/1, Content), " ")|")"]];
        splice_unquote -> ["(splice-unquote "| [string:join(lists:map(fun pr_str/1, Content), " ")|")"]]
    end;

pr_str(Atom) ->
    case is_integer(Atom) of
        false -> case is_function(Atom) of
                     true -> "*";
                     false -> Atom
                 end;
        true -> erlang:integer_to_list(Atom)
    end.
