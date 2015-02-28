-module(reader).

-export([read_str/1]).
%-export([tokenizer/1, read_form/1, peek/1, next/1]).
%-export([remove_empties/1]).

read_str(Str) ->
    {[Parsed], []} = read_form(tokenizer(Str)),
    Parsed.

peek([]) ->
    [];
peek([Token|Tokens]) ->
    {Token, [Token|Tokens]}.

next([]) ->
    [];
next([Token|Tokens]) ->
    {Token, Tokens}.

read_form(Tokens) ->
    case peek(Tokens) of
        [] -> "";
        {Token, Tokens2} ->
            case Token of
                [] -> io:format("end of file~n");
                "(" -> read_list(Tokens2);
                "'" -> read_quote(Tokens2);
                "`" -> read_quasiquote(Tokens2);
                "~" -> read_unquote(Tokens2);
                "~@" -> read_splice_unquote(Tokens2);
                _ -> read_atom(Tokens2)
            end
    end.

read_quote(Tokens) ->
    case next(Tokens) of
        {"'", Tokens2} -> {Read, Remaining} = read_form(Tokens2),
                          {[{quote, Read}], Remaining}
    end.

read_quasiquote(Tokens) ->
    case next(Tokens) of
        {"`", Tokens2} -> {Read, Remaining} = read_form(Tokens2),
                          {[{quasiquote, Read}], Remaining}
    end.

read_unquote(Tokens) ->
    case next(Tokens) of
        {"~", Tokens2} -> {Read, Remaining} = read_form(Tokens2),
                          {[{unquote, Read}], Remaining}
    end.

read_splice_unquote(Tokens) ->
    case next(Tokens) of
        {"~@", Tokens2} -> {Read, Remaining} = read_form(Tokens2),
                          {[{splice_unquote, Read}], Remaining}
    end.

read_list(Tokens) ->
    case next(Tokens) of
        [] -> io:format("Expected: (~n");
        {"(", Tokens2} -> {Read, Remaining} = read_list_internal(Tokens2),
                          {[{list, Read}], Remaining}
    end.

read_list_internal(Tokens) -> read_list_internal(Tokens, []).
read_list_internal([], _Acc) ->
    erlang:error("expected ')', got EOF");
read_list_internal(Tokens, Acc) ->
    {Token, Tokens2} = next(Tokens),
    case Token of
        ")" -> 
            {Acc, Tokens2};
        _ -> {Read, ToRead} = read_form([Token|Tokens2]),
             read_list_internal(ToRead, Acc ++ Read)
    end.

read_atom(Tokens) ->
    {Token, Tokens2} = next(Tokens),
    {[{symbol, Token}], Tokens2}.

tokenizer(String) ->
    remove_empties(re:split(String, "[ ,]*(~@|~|[[]{}()'`~^@]|\"(?:\\\\.|[^\\\\\"])*\"|;.*|[^\\s\\[\\]{}('\"`,;)]*)", [{return, list}])).
%[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"|;.*|[^\s\[\]{}('"`,;)]*)

remove_empties([[]]) ->
    [];
remove_empties([[]| Strings]) ->
    remove_empties(Strings);
remove_empties([String| Strings]) ->
    [String| remove_empties(Strings)].
