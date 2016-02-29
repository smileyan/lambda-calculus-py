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