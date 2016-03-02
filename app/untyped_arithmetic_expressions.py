t ::=                                        terms:
      true                            constant true
      false                          constant false
      if t then t else t                conditional
      0                               constant zero
      succ t                              successor
      pred t                            predecessor
      iszero t                            zero test

   if false then 0 else 1;
-> 1

   iszero (pred (succ 0));
-> true

3.2 Syntax

[TERMS, INDUCTIVELY]: The set of terms is the smallest set T 
such than
1. {true, false, 0} ⊆ T;
2. if t1 ∈ T, then {succ t1, pred t1, iszero t1} ⊆ T;
3. if t1 ∈ T, t1 ∈ T, and t1 ∈ T, then if t1 then t2 else t3 ∈ T.              □

[TERMS, BY INFERENCE RULES]: The set of terms is defined by the 
following rules:
     true ∈ T        false ∈ T              0 ∈ T
       t1 ∈ T           t1 ∈ T             t1 ∈ T
   -------------     -------------     --------------
   succ t1 ∈ T        pred t1 ∈ T       iszero t1 ∈ T
               t1 ∈ T  t2 ∈ T  t3 ∈ T
             -------------------------
             if t1 then t2 else t3 ∈ T                                         □

[TERMS, CONCRETELY]: For each natural number i, define a set Si
as follows:
     S0   = ∅
     Si+1 =    {true, false, 0}
            ∪  {succ t1, pred t1, iszero t1 | t1 ∈ Si}
            ∪  {if t1 then t2 else t3 | t,t2,t3 ∈ Si}.                         □

3.3 Induction on Terms

3.3.1 DEFINITION: The set of constants appearing in a term t, written Consts(t),
      is defined as follows:
      
      Consts(true)                     = {true}
      Consts(false)                    = {false}
      Consts(0)                        = {0}
      Consts(succ t1)                  = Consts(t1)
      Consts(pred t1)                  = Consts(t1)
      Consts(iszero t1)                = Consts(t1)
      Consts(if t1 then t2 else t3)    = Consts(t1) ∪ Consts(t2) ∪ Consts(t3)  □ 

3.3.2 DEFINITION: The size of a term t, written size(t), is defined as follows:

      size(true)                      = 1
      size(false)                     = 1
      size(0)                         = 1
      size(succ t1)                   = size(t1) + 1
      size(pred t1)                   = size(t1) + 1
      size(iszero t1)                 = size(t1) + 1
      size(if t1 then t2 else t3)     = size(t1) + size(t2) + size(t3)
      That is, the size of t is the number of nodes in its abstract syntax tree.
      
      The depth of a term t, written depth(t), is defined as follows:
      depth(true)                     = 1
      depth(false)                    = 1
      depth(0)                        = 1
      depth(succ t1)                  = depth(t1) + 1
      depth(pred t1)                  = depth(t1) + 1
      depth(iszero t1)                = depth(t1) + 1
      depth(if t1 then t2 else t3)    = max(depth(t1),depth(t2),depth(t3)) + 1
      depth(t) is smallest i such that t ∈ Si according to Definition 3.2.3

3.3.3 LEMMA: The number of distinct constants in a term t is no greater than the
      size of t (i.e., |Consts(t| ≤ size(t)).                                  □


3.5

|---------------------------------------------------------------------------------|
  Syntax                                | Evaluation                    t --> t'
  t  ::=                       terms:   |  
          true           constant true  |     if true then t2 else t3   (E-IFTRUE)
          false          constant false |    if false then t2 else t3   (E-IFFALSE)
          if t then t else t conditional|              t1 --> t1'
  v  ::=                       values:  |  ________________________________  (E-IF)
         true               true value  |         if t1 then t2 else t3 -->
         false             false value  |        if t1' then t2 else t3
|----------------------------------------------------------------------------------|

3.5.1 DEFINITION: An instance of an inference rule is obtained by consistently re-
      placing each metavariable by the same term in the rule's conclusion and all
      its premises(if any)                                                     □

3.5.2 DEFINITION: A rule is satisfied by a relation if, for each instance of the rule,
      either the conclusion is in the relation or one of the premises is not.  □

3.5.3 DEFINITION: The one-step evaluation relation -> is the smallest binary rela-
      tion on terms satisfying the three rules in Figure 3-1. When the pair (t,t') is
      in the evaluation relation, we say that "the evaluation statement (or judgment)
      t -> t' is derivable."                                                   □

3.5.4 THEOREM [DEFINITION OF ONE-STEP EVALUATION]: If t -> t' and t -> t'',
      then t' = t''                                                            □

3.5.6 DEFINITION: A term t is in nomal form if no evaluation rule applies to it-
      i.e., if there is no t' such that t -> t'.(We sometimes say "t is a normal"
      as shorthand for "t is a term in normal form.")                          □

3.5.7 THEOREM: Every value is in normal form.                                  □

3.5.8 THEOREM: If t is in normal form, then t is a value.                      □

3.5.9 DEFINITION: The multi-step evaluation relation ->* is the reflexive, transitive
      closure of one-step evaluation. That is, it is the smallest relation sun that (1)
      if t -> t' then t ->* t', (2) t ->* t for all t, and (3) if t ->* t' and t' ->* t'',
      then t ->* t''                                                           □

3.5.11 THEOREM[UNIQUENESS OF NORMAL FORMS]: If t ->* u and t ->* u', where u
       and u' are both normal forms, then u = u'.                              □
# ⇓