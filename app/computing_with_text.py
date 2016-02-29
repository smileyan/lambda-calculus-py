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

2. A reflexive and transitive relation R on a set S is called a preorder on S  
   Preorders are usually written using symbols ≤. We write s < t 
   ("s is trictly less than t") to mean s ≤ t ∧ s ≠ t.
   A preorder (on a set S) that is also antisymmetric is called a partial order
   on S. A partial order ≤ is called a total order if it also has the property
   that, for each s and t in S, either s ≤ t or t ≤ s.                         □

3. Suppose that ≤ is a partial order on a set S and s and t are elements of S.
   An element j ∈ S is said to be a join (or least upper bound) of s and t if
   1. s ≤ j and t ≤ j, and
   2. for any element k ∈ S with s ≤ k and t ≤ k, we have j ≤ k.
   An element m ∈ S is said to be a meet (or greatest lower bound) of s and t if
   1. m ≤ s and m ≤ t, and
   2. for any element n ∈ S with n ≤ s and n ≤ t, we have n ≤ m.               □

4. A reflexive, transitive, and symmetric relation on s set S is called
   an equivalence on S.                                                        □

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
