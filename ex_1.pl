s  -->  np(X),vp(X).


np(X)  -->  det,n(X).


vp(X)  -->  v,np(_).
vp(X)  -->  v.
   
det  -->  [the].
det  -->  [a].
   
n(s)  -->  [man].
n(p)  --> [men].
n(s)  -->  [apple].
n(s)  -->  [pear].
n(s)  -->  [woman].

v --> {Word,lex(Word,v)}.


lex(eats,v).
lex(eat,v).

