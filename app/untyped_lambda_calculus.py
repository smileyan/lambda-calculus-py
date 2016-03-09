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

3. 