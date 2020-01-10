simple_s --> term.
simple_s --> term, bi_oper, term.
simple_s --> left, s, right.
simple_s --> oper, left, s, right.
s --> simple_s.
s --> simple_s, oper, s.
bi_oper --> [v].
bi_oper --> [^].
oper --> [~].
term --> [p].
term --> [q].
left --> ['('].
right --> [')'].
