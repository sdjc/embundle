-module(sample).

% a simple lisp parser in Erlang
% 2011-9-18

p_lisp(Str) ->
    p_lists(Str).

p_lists(Str) ->
    Rest = p_list(Str),
    p_lists(Rest).

p_list(Str) ->
                                                % list ::= '(' elements ')'
    p_elements(Str).

p_elements(Str) ->
                                                % elements ::= element elements
    Rest = p_element(Str),
    p_elements(Rest).

p_element(Str) ->
                                                % element ::= list
                                                %          |  number
                                                %          |  string
                                                %          |  symbol
    list(Str),
    number(Str),
    string(Str),
    symbol(Str).
