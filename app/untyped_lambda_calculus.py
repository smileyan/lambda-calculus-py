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