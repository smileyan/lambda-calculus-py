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
