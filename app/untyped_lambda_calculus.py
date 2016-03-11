t ::=                                                    terms:
       x                                              variable
       λx.t                                        abstraction
       t t                                         application

Abstract and Concrete Syntax

s t u = (s t) u
      apply
     /     \
   apply    u
  /     \
 s       t 
 λx.λy.xyx
              λx
              |
              λy
              |
            apply
           /     \
         apply    x
        /     \
       x       y

Variabes and Metavariables

"The term λx.λy.xy has the form λz.s, where z = x and s = λy.xy,"

Scope

A term with no free variables is said to be closed; closed terms are also
called combinators. The simplest combinator, called the identity function,
  id = λx.x;
does nothing but return its argument.

Operational Semantics

(λx.t12)t2 -> [x -> t2]t12,

Folling Church, a term of the form (λx.t12)t2 is called a redex ("reducible expression"),
and the operation of rewriting a redex according to the above rule is called beta-reduction.

1. Under fullbeta-reduction, we might choose,for example, to begin with the
innermost redex, the do the one in the middle, then the outermost:

(λx.x) ((λx.x) (λz. (λx.x) z)),

id(id(λz. id z))
          ----
id(id(λz z))
   --------
id(λz.z)
--------
λz.z

2. Under the normal order strategy, the leftmost, outermost redex is always
reduced first. Under this strategy, the term above would be reduced as
follows:

id(id(λz. id z))
----------------
id(λz. id z)
-----------
λz. id z
    ----
λz.z

Under this strategy(and the ones below), the evaluation relation is actually a 
partial function: each term t evaluates in one step to at most one term t`.

3. The call by name strategy is yet more restrictive, allowing no reductions 
inside abstractions. Starting from the same term, we would perform the 
first two reductions as under normal-order, but then stop before the last 
and regard λz. id z as a normal form:

id(id(λz. id z))
----------------
id(λz. id z)
-----------
λz. id z

call by need

4. Most languages use a call by value strategy, in which only outermost re-
dexes are reduced and where a redex is reduced only when is right-hand
size has already been reduced to a value

id(id(λz. id z))
----------------
id(λz. id z)
-----------
λz. id z

5.2 Programming in the Lambda-Calculus

Multiple Arguments: currying

Church Booleans

tru = λt.λf.t;
fls = λt.λf.f;

# a combinator test
test = λl.λm.λn.lmn;

   test tru v w
=  (λl.λm.λn. l m n) tru v w  by difinition
-> (λm.λn. tru m n) v w       reducing the underlined redex
-> (λn. tru v n) w            reducing the underlined redex
-> tru v w                    reducing the underlined redex
=  (λt.λf.t) v w              by difinition
-> (λf.v) w                   reducing the underlined redex
-> v                          reducing the underlined redex

and = λb.λc. b c fls;

and tru tru;

=  (λb.λc. b c fls) tru tru
-> tru tru fls                 reducing the underlined redex
-> λt.λf.t tru fls             by difinition
-> λf.tru fls                  reducing the underlined redex
-> tru                         reducing the underlined redex
