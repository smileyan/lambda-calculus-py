1.1 Defining Sets

B = t
  | f
  | (B.B)

t ∈ B
f ∈ B
a ∈ B and b ∈ B ⇒ (a.b) ∈ B

t ∈ B       [a]
f ∈ B       [b]
(B1.B2) ∈ B [c]

B = {t,f,(t.f),(t,t),...}

For (t.(f.t))

1. t ∈ B          by [a]
2. f ∈ B          by [b]
3. t ∈ B          by [a]
4. (f.t) ∈ B      by 2, 3, and [c]
4. (t.(f.t)) ∈ B  by 1, 4, and [c]

proof-tree form:
   f ∈ B [b]  t ∈ B [2]
   ---------------------
t ∈ B [a]    (f.t) ∈ B [c]
---------------------------
     (t.(f.t)) ∈ B     [c]

1.2 Relations: A relation is a set whose elements consist of ordered pairs.

a ∈ B ⇒ <a,a> ∈ ≡
a ∈ B ⇒ a ≡ a

a relation R is reflexive iff aRa (for any a)
a relation R is symmetric iff aRb ⇒ bRa
a relation R is transitive iff aRb and bRc ⇒ aRc

If a relation is reflexive, symmetric, and transitive, then it is an equivalence.

neither reflexive, symmetric, nor transitive:
(f.B1) r B1 [a]
(t.B1) r t  [b]

Ordered Sets

1. reflexive symmetric transitive                                              □

Sequences

Induction

1. Axiom[principle of ordinar induction on natural numbers]:
    if P(0)
    and, for all i, P(i) implies P(i+1),
    then P(n) holds for all n.                                                 □

2. Axiom[principle of complete induction on natural number]:
    if, for each natural number n,
        given P(i) for i < n,
        we can show P(n),
    then P(n) holds for all n.                                                 □

3. dictionary order on pairs of natural numbers is defined as follows :
   (m, n) ≤ (m', n') iff either m < m' or else m = m' and n ≤ n'.              □

4. Aiom[principle of lexicographic induction]:
    if, for each pair (m, n) of natural numbers,
        given P(m', n') for all (m', n') < (m, n)
    then P(m, n) holds for all m, n.                                           □


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

Syntax

[TERMS, INDUCTIVELY]: The set of terms is the smallest set T 
such than
1. {true, false, 0} ⊆ T;
2. if t1 ∈ T, then {succ t1, pred t1, iszero t1} ⊆ T;
3. if t1 ∈ T, t1 ∈ T, and t1 ∈ T, then if t1 then t2 else t3 ∈ T.              □

[TERMS, BY INFERENCE RULES]: The set of terms is defined by the 
following rules:
         true ∈ T        false ∈ T        0 ∈ T
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