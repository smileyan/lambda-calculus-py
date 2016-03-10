2A Introduction to CL
A(x,y) = x + y (for all x,y)
(C(f))(x,y) = f(y,x)
A = C(A)

B, which composes two functions: (B(f,g))(x) = f(g(x));
B`, a reversed composition operator: (B`(f,g))(x) = g(f(x));
I, the identify operator: I(f) = f;
K, which forms constant functions: (K(a))(x) = a;
S, a stronger composition operator: (S(f,g))(x) = f(x,g(x));
W, for doubling or 'diagonalizing': (W(f))(x) = f(x,x).

Defination 2.9 (Weak reduction) Any term IX, KXY or SXYZ is called a (weak)
redex. Contracting an occurence of a weak redex in a term U means replacing
one occurence of 
              IX by X,     or
             KXY by X,     or
            SXYZ by XZ(YZ).

Defination 2.10 A Weak normal form (or weak nf or term in weak normal form)
is a term that contains no weak redexes. Iff a term U weakly reduces to a
weak normal form X, we call X a weak normal form of U.

Example 2.11 Define B ≡ S(KS)K. Then BXYZ▷ω X(YZ) for all terms X,Y and Z

BXYZ ≡    S(KS)KXYZ 
     ▷1ω  KSX(KX)YZ by contracting S(KS)KX to KSX(KX)
     ▷1ω  S(KX)YZ   by contracting SKX TO S
     ▷1ω  KXZ(YZ)   by contracting S(KX)YZ
     ▷1ω  X(YZ)     by contracting KXZ.

Lemma 2.14 (Substitution lemma for ▷ω)
  (a) X ▷ω Y => FV(X) > FV(Y);
  (b) X ▷ω Y => [X/v]Z ▷ω [Y/v]Z;

Theorem 2.15 (Church-Rosser theorem for ▷ω) If U ▷ω X and
U ▷ω Y, then there exists a CL-term T such that
          X ▷ω T and Y ▷ω T.

Corollary 2.15.1 (Uniqueness of nf) A CL-term can have at most
on weak normal form.

2C Abstraction in CL

Defination 2.18 (Abstraction) For every CL-term M and every
variable x, a CL-term called [x].M is defined by induction on M, thus:

 (a) [x].M  = KM               if x ≠ FV(M;
 (b) [x].x  = I;
 (c) [x].Ux = U                if x ≠ FV(U)
 (f) [x].UV = S([x].U)([x].V)  if neither (a) nor (c) applies.
 
 Example 2.19
         [x].xy = S([x].x)([x].y)    by 2.18 (f)
                = SI(Ky)             by 2.18 (b) and (a)

Warning 2.20 In lc an expression lambdax can be part of a lambda term,
for example the term lambda(x).xy . But in CL, the corresponding expression
[x] is not part of the formal language of CL-term

Lemma 2.28 (Substitution and abstraction)
  (a) FV([x].M) = FV(M) - {x}        if x belong to FV(M)