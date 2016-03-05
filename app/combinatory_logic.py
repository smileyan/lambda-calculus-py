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