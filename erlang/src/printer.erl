-module(printer).

-export([pr_str/2, pr_str/1]).

pr_str(PrintReadability) ->
    fun(Atom) -> pr_str(Atom, PrintReadability) end.

%pr_str([], _PrintReadability) ->
%    "\"\"";
pr_str({Type, Content}, PrintReadability) ->
    PrStr = pr_str(PrintReadability),
    case Type of
        symbol -> Content;
        nil -> "";
        function -> "*";
        list -> ["("| [string:join(lists:map(PrStr, Content), " ")|")"]];
        vector -> ["["| [string:join(lists:map(PrStr, Content), " ")|"]"]];
        hashmap -> ["{"| [string:join(lists:map(PrStr, Content), " ")|"}"]];
        quote -> ["(quote "| [string:join(lists:map(PrStr, Content), " ")|")"]];
        quasiquote -> ["(quasiquote "| [string:join(lists:map(PrStr, Content), " ")|")"]];
        unquote -> ["(unquote "| [string:join(lists:map(PrStr, Content), " ")|")"]];
        splice_unquote -> ["(splice-unquote "| [string:join(lists:map(PrStr, Content), " ")|")"]]
    end;

pr_str(Atom, PrintReadability) ->
    case is_integer(Atom) of
        false -> case is_function(Atom) of
                     true -> "*";
                     false -> Str = printable_string(Atom),
                              Str
                 end;
        true -> erlang:integer_to_list(Atom)
    end.

printable_string(Atom) ->
    case is_list(Atom) of
        true ->
            Atom2 = re:replace(Atom, "(?!^\")(?!\"$)\"", "\\\\\"", [global, {return, list}]),
            Atom3 = re:replace(Atom2, "\~n", "\\\\n", [global, {return, list}]),
            Atom3;
        false ->
            Atom
    end.
