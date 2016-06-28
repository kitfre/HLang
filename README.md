# HLang
A little list based programming langauge inspired by languages like J written in Haskell.

Basic syntax:
[1 2 3] : a list

Let a, b be lists.

!0a : index 0th element from a

?1a : is 1 an element of a?

+a  : fold sum a

*a  : fold multiply a

++ab : concatenate a b

if[bool a b] : if statement

==[1 2] : comparators, including <, >, <=, >=, ==

->f[x . x+1] : defines a function f that takes a single parameter and increments it

f[1] : function application
