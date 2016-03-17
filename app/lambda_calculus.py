# lambda calculus was devised by Alonzo Church in the 1930's

# lambda expressions: 
# <expression> ::= <name> | <function> | <application>
# lambda function:
# <function> ::= λ<name>.<body>
    # where <body> ::= <expression>
# <application> ::= (<function expression> <argument expression>)
    # where
    #     <function expression> ::= <expression>
    #     <argument expression> ::= <expression> 

1B Term-structure and subsitution

Definition 1.6 The length of a term M (called lgh(M)) is the total
number of occurrences of atoms in M. In more detail, define

  (a) lgh(a)    = 1                 for atoms a;
  (b) lgh(MN)   = lgh(M) + lgh(N);
  (c) lgh(λx.M) = 1 + lgh(M).

Definition 1.12 (Substitution) For any M,N,x define [N/x]M to 
be the result of subsituting N for every free occurrence of x in M, and
changing bound variables to avoid clashes.
  (a) [N/x]x      ≡ N;
  (b) [N/x]a      ≡ a               for all atoms a ≠ x;
  (c) [N/x](PQ)   ≡ ([N/x]P [N/x]Q);
  (d) [N/x](λx.P) ≡ λx.P;
  (e) [N/x](λy.P) ≡ λy.P            if x ∉ FV(P);
  (f) [N/x](λy.P) ≡ λy.[N/x]p       if x ∈ FV(P) and y ∉ FV(N);
  (g) [N/x](λy.P) ≡ λz.[N/x][z/y]P  if x ∈ FV(P) and y = FV(N).

Lemma 1.15 For all terms M, N and variables x:
  (a) [x/x]M ≡ M;
  (b) x ∉ FV(M) ->

Definition 1.24 (-reducing) Any term of form
         (λx.M)N
is called a -redex and the corresponding term
         [N/x]M
is called its contractum

Definition 1.26 A term Q which contains no β-redexed is called a 
β-normal form (or a term is β-normal form or just a β-nf)

Remark 1.29 Some terms can be reduced in more than one way.
One such term, from Example 1.25(c), is (λx.(λy.yx)z)v 1β 

The Church-Rosser theorem below will show that the normal form of
a term is indeed unique, provided we ignore changes of bound variables.

Lemma 1.30 P β Q => FV(P) > FV(Q)

Lemma 1.31 (Substitution and β) If P β P` and Q β Q`, then
           [P/x]Q β [P`/x]Q`.

Theorem 1.32 (Church-Rosser theorem for β) If P β M and
P β N (see Figure 1:1), then there exists a term T such that
           M β T and N β T.
           
Corollary 1.32.1 If P has a β-normal form, it is unique modolo = a ;
that is, if P has β-normal forms M and N, then M a N.

identify = lambda x: x

def self_apply():
    return lambda s: s(s)

apply = lambda func: lambda arg: func(arg)

select_first = lambda first: lambda second: first

select_second = lambda first: lambda second: second

make_pair = lambda first: lambda second: lambda func: func(first)(second)

# Truth values and conditional expression
#     <condition>?<expression>:<expression>
cond = lambda e1: lambda e2: lambda c: c(e1())(e2())

true = select_first

false = select_second

# NOT <operand>
_not = lambda x: x(false)(true)

# AND <operand> AND <operand>
_and = lambda x: lambda y: x(y)(false)

# OR <operand> OR <operand>
_or = lambda x: lambda y: x(true)(y)

# zero  = lambda f: lambda z: z
# one   = lambda f: lambda z: f(z)
# two   = lambda f: lambda z: f(f(z))
# three = lambda f: lambda z: f(f(f(z)))

succ  = lambda n: lambda s: s(false)(n)
iszero = lambda n: n(select_first)
pred = lambda n: iszero(n)(zero)(n(select_second))

zero  = identify
one   = succ(zero)
two   = succ(one)
three = succ(two)

# U Combinator
U = lambda f: f(f)

add  = U(lambda f: lambda a: lambda b: a if b <= 0 else 1 + U(f)(a)(b-1))
mul  = U(lambda f: lambda a: lambda b: 0 if b <= 0 else a + U(f)(a)(b-1))
pow  = U(lambda f: lambda a: lambda b: 1 if b <= 0 else a * U(f)(a)(b-1))
sub  = U(lambda f: lambda a: lambda b: a if b <= 0 else 0 + U(f)(a-1)(b-1))
fact = U(lambda f: lambda n: 1 if n <= 0 else n*(U(f))(n-1))

equal = U(lambda f: lambda a: lambda b: True if ((a==0) and (b==0)) else (False if (a==0) or (b==0) else U(f)(a-1)(b-1)))
greater = lambda a: lambda b: sub(a)(b) > 0

# Y Combinator: x = f(x)
# The fixed point of a function f is the value x such that x=f(x)
# Y(F) == F(Y(F))
# Y = lambda F: F(Y(F))
# e == lambda x: e(x)
# Y = lambda F: F(lambda x:Y(F)(x))
# apply the U combinator to eliminate the explicit recursion in the definition of Y
Y = U(lambda h: lambda F: F(lambda x:U(h)(F)(x)))

fact_y = Y(lambda f: lambda n: 1 if n <= 0 else n*f(n-1))
add_y  = Y(lambda f: lambda a: lambda b: a if b <= 0 else 1 + f(a)(b-1))

# types
# 1) construct an object from a value and a type 
# 2) select the value and type from an object
# 3) test the type of an object
# 4) handle type errors

def make_obj(type):
    return lambda value: lambda s: s(type)(value)

def type(obj):
    return obj(select_first)

def value(obj):
    return obj(select_second)

def istype(t):
    return lambda obj: equal(type(obj))(t)

# Errors
error_type = zero

MAKE_ERROR = make_obj(error_type)

ERROR = MAKE_ERROR(error_type)

def is_error(obj):
    return istype(error_type)(obj)

# Boolean
bool_type = one

MAKE_BOOL = make_obj bool_type # λvalue.λs.(s bool_type value)

TRUE = MAKE_BOOL(true) # λs.(s bool_type true)

FALSE = MAKE_BOOL(false) # λs.(s bool_type false)

isbool = istype bool_type # λobj.(equal (type obj) bool_type)

BOOL_ERROR = MAKE_ERROR bool_type # λs.(s error_type bool_type)

def NOT(X):
    if isbool(X):
        MAKE_BOOL(not(value(X)))
    else:
        BOOL_ERROR

def AND(X, Y):
    if and(isbool(X))(isbool(Y)):
        MAKE_BOOL(and(value(X))(value(Y)))
    else:
        BOOL_ERROR

# isbool TRUE ==
# λobj.(equal (type obj) bool_type) TRUE =>
# equal (type TRUE) bool_type ==
# equal (λobj.(obj select_first) TRUE) bool_type ->
# equal (TRUE select_first) bool_type ==
# equal (λs.(s bool_type true) select_first) bool_type -> ... ->
# equal bool_type bool_type
# true

# Typed conditional expression

def COND(E1, E2, C):
    if isbool(C):
        if value(C):
            return E1
        else:
            return E2
    else:
        return BOOL_ERROR

def ISERROR(E):
    return MAKE_BOOL(is_error(E))

def ISBOOL(B):
    return MAKE_BOOL(isbool(B))

# Numbers and arithmetic
numb_type = two

MAKE_NUMB = make_obj(numb_type)
# λvalue.λs.(s numb_type value)

NUMB_ERROR = MAKE_ERROR(numb_type)
# λs(s error_type numb_type)

isnumb = istype(numb_type)
# λobj.(equal (type obj) numb_type)

def ISNUMB(N):
    return MAKE_BOOL(isnumb(N))

_0 = MAKE_NUMB zero
# λs.(s numb_type zero)

def SUCC(N):
    if isnumb(N):
        return MAKE_NUMB(succ(value(N)))
    else:
        return NUMB_ERROR

_1 = SUCC _0
# etc.

def PRED(N):
    if isnumb(N):
        if iszero(value(N)):
            return NUMB_ERROR
        else:
            return MAKE_NUMB(value(N))(select_second)
    else:
        return NUMB_ERROR

def ISZERO(N):
    if isnumb(N):
        return MAKE_BOOL(iszero(value(N)))
    else:
        return NUMB_ERROR

def both_numbs(x, y):
    return and(isnumb(x))(isnumb(y))

def _+(x, y):
    if both_numbs(x, y):
        return MAKE_NUMB(add(value(x))(value(y)))
    else:
        return NUMB_ERROR

def _*(x, y):
    if both_numbs(x, y):
        return MAKE_NUMB(mul(value(x))(value(y)))
    else:
        return NUMB_ERROR

def _/(x, y):
    if both_numbs(x, y):
        if iszero(value(y)):
            return NUMB_ERROR
        else:
            return MAKE_NUMB(div1(value(x)(value(y))))

def EQUAL(X, Y):
    if both_numbs(X, Y):
        return MAKE_BOOL(equal(value(X))(value(Y)))
    else:
        return NUMB_ERROR

char_type = four

CHAR_ERROR = MAKE_ERROR char_type

ischar = istype char_type

def ISCHAR(C):
    return MAKE_BOOL(ischar(C))
    
MAKE_CHAR = make_obj char_type

_0 = MAKE_CHAR(forty_eight)
# ...
_9 = MAKE_CHAR(succ(value(8)))
_A = MAKE_CHAR(sixty_five)
# ...
_Z = MAKE_CHAR(succ(value('Y')))
_a = MAKE_CHAR(ninety_seven)
# ...
_z = MAKE_CHAR(succ(value('y')))

def CHAR_LESS(C1, C2):
    if ischar(C1) && ischar(C2):
        return MAKE_BOOL(less(value(C1), value(C2)))
    else:
        return CHAR_ERROR

def ORD(C):
    if ischar(C):
        return MAKE_NUMB(value(C))
    else:
        CHAR_ERROR

def CHAR(N):
    if isnumb(N):
        return MAKE_CHAR(value(N))
    else:
        return NUMB_ERROR

# ORD 'A' => ... =>
# MAKE_NUMB (value 'A') ==
# MAKE_NUMB (value λs.(s char_type sixty_five)) -> ... ->
# MAKE_NUMB sixty_five
# λs.(numb_type sixty_five) == 
# 65

# CHAR 97 => ... =>
# MAKE_CHAR (value 97) ==
# MAKE_CHAR (value λs.(s numb_type ninety_seven)) -> ... ->
# MAKE_CHAR ninety_seven => ... =>
# λs.(char_type ninety_seven) ==
# 'a'

def CHAR_EQUAL(C1, C2):
    if ischar(C1) and ischar(C2):
        return MAKE_BOOL(equal(value(C1), value(C2)))
    else:
        return CHAR_ERROR


# List representation
list_type = three
def islist():
    return istype(list_type)

def ISLIST(L):
    return MAKE_BOOL(islist(L))

def LIST_ERROR():
    return MAKE_ERROR(list_type)

def MAKE_LIST = make_obj list_type

def CONS(H, T):
    if islist(T):
        return MAKE_LIST(lambda s: s(H)(T))
    else:
        return LIST_ERROR

# CONS 1 NIL => ... =>
# MAKE_LIST λs.(s 1 NIL) => ... =>
# λs.(s list_type
#          λs.(s 1 NIL))

def NIL():
    return MAKE_LIST(lambda s: s(LIST_ERROR)(LIST_ERROR))

# λs.(s list_type
#         λs.(s LIST_ERROR LIST_ERROR))
def HEAD(L):
    if islist(L):
        return value(L)(select_first)
    else:
        return LIST_ERROR

def TAIL(L):
    if islist(L):
        return value(L)(select_second)
    else:
        LIST_ERROR

def LENGTH(L):
    if isnil(L):
        return 0
    else:
        return SUCC(LENGTH(TAIL L))

def APPEND(L1, L2):
    if isnil(L1):
        return L2
    else:
        return CONS(HEAD(L1), APPEND(TAIL(L1), L2))

# DELETE X [] = []
# DELETE X (H::T) = T if <equal> X H
# DELETE X (H::T) = H::(DELETE X T) if NOT (<equal> X H)

def DELETE(V, L):
    if isnil(L):
        return NIL
    else:
        if equal(v, HEAD(L)):
            return TAIL(L)
        else:
            return CONS(HEAD(L), DELETE(V, TAIL(L)))

# LIST_EQUAL [] [] = TRUE
# LIST_EQUAL [] (H::T) = FALSE

# LIST_EQUAL (H1::T1) (H2::T2) = LIST_EQUAL T1 T2
#                                if <equal> H1 H2
# LIST_EQUAL (H1::T1) (H2::T2) = FALSE
#                                if NOT (<equal> H1 H2)

def LIST_EQUAL(L1, L2):
    if isnil(L1) and isnil(L2):
        return TRUE
    else:
        if isnil(L1) or isnil(L2):
            return FALSE
        else:
            if EQUAL(HEAD(L1), HEAD(L2)):
                return LIST_EQUAL(TAIL(L1), TAIL(L2))
            else:
                return FALSE

# Strings: linear lists of characters.

# ISSTRING [] = TURE
# ISSTRING (H::T) = (ISCHAR H) AND (ISSTRING T)

# rec ISSTRING S =
#   IF ISNIL S
#   THEN TRUE
#   ELSE AND (ISCHAR (HEAD S)) (ISSTRING (TAIL S))

# String comparison

# STRING_EQUAL "" "" = TRUE
# STRING_EQUAL "" (C::S) = FALSE
# STRING_EQUAL (C::S) "" = FALSE

# STRING_EQUAL (C1::S1) (C2::S2) = STRING_EQUAL S1 S2
#                                  if CHAR_EQUAL C1 C2

# STRING_EQUAL (C1::S1) (C2::S2) = FALSE
#                                  if NOT (CHAR_EQUAL C1 C2)

# rec STRING_EQUAL S1 S2 =
#   IF (ISNIL S1) AND (ISNIL S2)
#   THEN TRUE
#   ELSE
#     IF (ISNIL S1) OR (ISNIL S2)
#     THEN FALSE
#     ELSE
#       IF CHAR_EQUAL (HEAD S1) (HEAD S2)
#       THEN STRING_EQUAL (TAIL S1) (TAIL S2)
#       ELSE FALSE

# STRING_LESS "" (C::S) = TRUE
# STRING_LESS (C::S) "" = FALSE

# STRING_LESS (C1::S1) (C2::S2) = TRUE
#                                 if CHAR_LESS C1 C2

# STRING_LESS (C1::S1) (C2::S2) = (CHAR_EQUAL C1 C2) AND
#                                 (STRING_LESS S1 S2)
#                                 if NOT (CHAR_LESS C1 C2)

# rec STRING_LESS S1 S2 =
#   IF ISNIL S1
#   THEN NOT (ISNIL S2)
#   ELSE
#     IF ISNIL S2
#     THEN FALSE
#     ELSE
#       IF CHAR_LESS (HEAD S1) (HEAD S2)
#       THEN TRUE
#       ELSE (CHAR_EQUAL (HEAD S1) (HEAD S2)) AND
#            (STRING_LESS (TAIL S1) (TAIL S2))

# Numeric string to number converion

# value '0' => ... =>
# forty_eight

# sub (value '0') (value '0') -> ... ->
# sub forty_eight forty_eight => ... ->
# zero

# value '1' => ... =>
# forty_nine

# sub (value '1') (value '0') -> ... ->
# sub forty_nine forty_eight => ... =>
# one

# def digit_value d = sub (value d) (value '0')

# STRING_VAL V "" = V
# STRING_VAL V D::T = STRING_VAL 10*V + (digit_value D) T

# STRING_VAL 0 "321" ==
# STRING_VAL 0 ('3'::"21") -> ... ->
# STRING_VAL 10*0 + (digit_value '3') "21" -> ... ->
# STRING_VAL 3 "21" ==
# STRING_VAL 3 ('2'::"1") -> ... ->
# STRING_VAL 10*3 + (digit_value '2') "1" -> ... ->
# STRING_VAL 32 "1" == 
# STRING_VAL 32 ('1'::"") -> ... ->
# ...
# 321

# rec string_val v L =
#   IF ISNIL L
#   THEN v
#   ELSE string_val (add (mult v ten) (digit_value (HEAD L)))
#                   (TAIL L)

# def STRING_VAL S = MAKE_NUMB (string_val zero S)

rec <name> <bound variable> = 
  IF ISNIL <bound variable>
  THEN <expression1>
  ELSE <expression2> using (HEAD <bound variable>)
                     and   (TAIL <bound variable>)

rec <name> [] = <expression1>
 or <name> (<head>::<tail>) = <expression2 using <head>
                                             and <tail>>

LENGTH [] = 0
LENGTH (H::T) = SUCC (LENGTH T)

rec LENGTH L =
  IF ISNIL L
  THEN 0
  ELSE SUCC (LENGTH (TAIL L))

rec LENGTH [] = 0
 or LENGTH (H::T) = SUCC (LENGTH T)

FLAT [] = []
FLAT (H::T) = H::(FLAT T) if NOT (ISLIST H)
FLAT (H::T) = APPEND (FLAT H) (FLAT T) if (ISLIST H)

rec FLAT L =
 IF ISNIL L 
 THEN []
 ELSE
   IF NOT (ISLIST (HEAD L))
   THEN (HEAD L)::(FLAT (TAIL L))
   ELSE APPEND (FLAT (HEAH H)) (FLAT (TAIL L))

# Ordered linear lists, insertion and sorting

ORDERED [] = TRUE

ORDERED [C] = TRUE

ORDERED (C1::C2::L) = (<less> C1 C2) AND (ORDERED (CONS C2 L))

INSERT X [] = [X]
INSERT X (H::T) = X::H::T
                  if <less> X H

INSERT X (H::T) = H::(INSERT X T)
                  if NOT <less> X H

rec INSERT S [] = [S]
 or INSERT S (H::T) =
  IF STRING_LESS S H
   THEN S::H::T
   ELSE H::(INSERT H T)

SORT [] = []
SORT (H::T) = INSERT H (SORT T)

rec SORT [] = []
 or SORT (H::T) = INSERT H (SORT T)
 
rec IFIND N [] = []
 or IFIND 0 (H::T) = H
 or IFIND (SUCC N) (H::T) = IFIND N T

rec IDELETE N [] = []
 or IDELETE 0 (H::T) = H
 or IDELETE (SUCC N) (H::T) = H::(IDELETE N T)

rec IBEFORE N E [] = []
 or IBEFORE 0 E L = E::L
 or IBEFORE (SUCC N) E (H::T)) = H::(IBEFORE N E T)

rec IREPLACE N E [] = []
 or IREPLACE 0 E (H::T) = E::T
 or IREPLACE (SUCC N) E (H::T) = H::(IREPLACE N E T)

rec DOUBLE [] = []
 or DOUBLE (H::T) = (2*H)::(DOUBLE T)

rec PLURAL [] = []
 or PLURAL (H::T) = (APPEND H 's')::(PLURAL T)

rec MAPCAR FUNC [] = []
 or MAPCAR FUNC (H::T) = (FUNC H)::(MAPCAR FUNC T)

rec MAPCARS FUNC [] [] = []
 or MAPCARS FUNC (H1::T1) (H2::T2) = (FUNC H1 H2)::(MAPCARS FUNC T1 T2)

def COMP = MAPCARS λX.λY.(STRING_EQUAL X Y)
def SUMS = MAPCARS λX.λY.(X + Y)

COMPOSITE VALUES AND TREES

Composite values

rec NFIND S [] = []
 or NFIND S (H::T) =
  IF STRING_EQUAL S (HEAD (TAIL H))
  THEN HEAD H
  ELSE NFIND S T

rec SCHECK [] = []
 or SCHECK (H::T) =
  IF LESS (HEAD (TAIL H)) (HEAD (TAIL (TAIL H)))
  THEN H::(SCHECK T)
  ELSE SCHECK T

rec DCHANGE S N [] = []
 or DCHANGE S N (H::T) =
 IF STRING_EQUAL S (HEAD (TAIL (HEAD H)))
 THEN [(HEAD H), (HEAD TAIL H), N]::T
 ELSE H::(DCHANGE S N T)

Selector functions

def FORENAME N = HEAD N
def SURNAME N = HEAD (TAIL N)

def ITEM N = HEAD N
def STOCK N = HEAD (TAIL N)
def REORDER N = HEAD (TAIL (TAIL N))

def NAME E = HEAD E
def EFORENAME E = FORENAME (NAME E)
def ESURNAME E = SURNAME (NAME E)
def ADDRESS E = HEAD (TAIL E)
def PHONE E = HEAD (TAIL (TAIL E))

rec NDELETE S [] = []
 or NDELETE S (H::T) =
  IF STRING_EQUAL S (SURNAME H)
  THEN T
  ELSE H::(NDELETE S T)

rec SINCREMENT I V [] = []
 or SINCREMENT I V (H::T) =
  IF STRING_EQUAL I (ITEM H)
  THEN [(ITEM H), (STOCK H) + V, (REORDER H)]::T
  ELSE H::(SINCREMENT I V T)

rec DINSERT E [] = [E]
 or DINSERT E (H::T) =
 IF STRING_EQUAL (ESURNAME E) (ESURNAME H)
 THEN E::H::T
 ELSE H::(DINSERT E T)
 
 def FORENAME [F,S] = F
 def SURNAME [F,S] = S
 
 [F,S] = F::S::NIL
 [<forename>,<surname>] == <forename>::<surname>::NIL
 F == <forename>
 S == <surname>
 ([F,S]::T)
 
 rec NCOUNT N [] = 0
  or NCOUNT N ([F,S]::T) = 
   IF EQUAL N F
   THEN 1 + (NCOUNT N T)
   ELSE (NCOUNT N T)

def ITEM [I,S,R] = I
def STOCK [I,S,R] = S
def REORDER [I,S,R] = R
[I,S,R] == I::S::R::NIL
[<item name>,<stock level>,<reorder level>] ==
 <item name>::<stock level>::<reorder level>::NIL

I == <item name>
S == <stock level>
R == <reorder level>

([I,S,R]::T)

rec REORD [] = []
 or REORD ([I,S,R]::T) =
 IF LESS S R
 THEN [I,S,T]::(REORD T)
 ELSE REORD T

def ENAME [N,A,P] = N
def EFORENAME [[F,S],A,P] = F
def ESURNAME [[F,S],A,P] = S
def ADDRESS [N,A,P] = A
def PHONE [N,A,P] = P

[N,A,P] == N::A::P::NIL

[<name>,<address>,<number>] == <name>::<address>::<number>::NIL

N == <name>
A == <address>
P == <number>

[[F,S],A,P] == (F::S::NIL)::A::P::NIL

[[<forename>,<surname>],<address>,<number>] == (<forename>::<surname>::NIL)::<address>::<number>::NIL

F == <forename>
S == <surname>
[N,A,P]::T

rec DINSERT R [] = [R]
 or DINSERT [N1,A1,P1] ([N2,A2,P2]::T) =
  IF LESS P1 P2
  THEN [N1,A1,P1]::[N2,A2,P2]::T
  ELSE [N2,A2,P2]::(DINSERT [N1,A1,P1] T)

rec DSORT [] = []
 or DSORT (H::T) = DINSERT H (DSORT T)
 
Local definitions

λ<name>.<body> <argument

let <name> = <argument>
in <body>

<body>
where <name> = <argument>

rec SPLIT [] = []::[]
 or SPLIT ([F,S]::L) =
  let (FLIST::SLIST) = SPLIT L
  in ((F::FLIST))::(S::SLIST)

Tree

nodes, branches sub-trees root leaf N-ary

binary left right

EMPTY is a binary trees

NODE ITEM L R is a binary tree 
if L is a binary tree and R is a binary tree

def EMPTY = NIL
def ISEMPTY = ISNIL

def NODE ITEM L R = [ITEM,L,R]

ITEM (NODE I L R) = I
LEFT (NODE I L R) = L
RIGHT (NODE I L R) = R

ITEM EMPTY = TREE_ERROR
LEFT EMPTY = TREE_ERROR
RIGHT EMPTY = TREE_ERROR

def TREE_ERROR = LIST_ERROR
def ITEM EMPTY = TREE_ERROR
 or ITEM [I,L,R] = I
def LEFT EMPTY = TREE_ERROR
 or LEFT [I,L,R] = L
def RIGHT EMPTY = TREE_ERROR
 or RIGHT [I,L,R] = R

TADD I EMPTY = NODE I EMPTY EMPTY
TADD I (NODE NI L R) = NODE NI (TADD I L) R
                       if <less> I NI
TADD I (NODE NI L R) = NODE NI L (TADD I L)
                       if NOT (<less> I NI)

rec TADD I EMPTY = NODE I EMPTY EMPTY
 or TADD I [NI,L,R] =
 IF LESS I NI
 THEN NODE NI (TADD I L) R
 ELSE NODE NI L (TADD I R)

rec TADDLIST [] TREE = TREE
 or TADDLIST (H::T) TREE = TADDLIST T (TADD H TREE)

Binary tree traversal

TRAVERSE (NODE I L R) = APPEND (TRAVERSE L) (I::(TRAVERSE R))
TRAVERSE EMPTY = []

rec TRAVERSE EMPTY = []
 or TRAVERSE [I,L,R] = APPEND (TRAVERSE L) (I::(TRAVERSE R))

Binary tree search

TFIND V EMPTY = FALSE
TFIND V (NODE NV L R) = TRUE if <equal> V NV
TFIND V (NODE NV L R) = TFIND V L if <less> V NV
TFIND V (NODE NV L R) = TFIND V R if NOT (<less> V NV)

rec TFIND V EMPTY = ""
 or TFIND V [NV,L,R] =
  IF EQUAL V NV
  THEN TRUE
  ELSE
   IF LESS V NV
   THEN TFIND V L
   ELSE TFIND V R

Binary trees of composite values

rec CTADD N EMPTY = [N,EMPTY,EMPTY]
 or CTADD [F,S] [[NF,NS],L,R] =
  IF STRING_LESS S NS
  THEN [[NF,NS],(CTADD [F,S] L),R]
  ELSE [[NF,NS],L,(CTADD [F,S] R)]

rec CTADDLIST [] TREE = TREE
 or CTADDLIST (H::T) TREE = CTADDLIST T (CTADD H TREE)

rec CTFIND S EMPTY = ""
 or CTFIND S [[NF,NS],L,R] =
  IF STRING_EQUAL S NS
  THEN NF
  ELSE
   IF STRING_LESS S NS
   THEN CTFIND S L
   ELSE CTFIND S R

Curried and uncurried functions

def curry f x y = f [x,y]

def SUM_SQ1 [X,Y] = (X*X)+(Y*Y)

def curry_SUM_SQ = curry SUM_SQ1

λf.λx.λy.(f [x,y]) SUM_SQ1 =>
λx.λy.(SUM_SQ1 [x,y])

def curry_SUM_SQ x y= SUM_SQ1 [x,y]

def uncurry g [a,b] = g a b

def SUM_SQ2 X Y = (X*X)+(Y*Y)

def uncurry_SUM_SQ = uncurry SUM_SQ2

λg.λa.λb.(g a b) SUM_SQ2 =>
λ[a,b].(SUM_SQ2 a b)

Evaluation

Introduction

applicative and normal order evaluation

halting problem

Church-Rosser theorems

lazy evaluation

Termination and normal form

λx.x λa.(a a) and λf.λa(f a) λs.(s s)

λx.x λa.(a a) =>
λa.(a a)

λf.λa(f a) λs.(s s) =>
λa.(λs.(s s) a) =>
λa.(a a)

Normal order

Applicative order

Consistent applicative order use

Delaying evaluation

Evaluation termination, the halting problem, evaluation equivalance and the Church-Rosser theorems

Normal and applicative are interchangeable but normal order gives a better guarantee of evaluation termination

1. Every expression has a unique normal form.
2. If an expression has a normal form then it may be reached by normal order evaluation.

Infinite objects

def cons h t s = s h t
def head l = l λx.λy.x
def tail l = l λx.λy.y

rec numblist n = cons n (numblist (succ n))
def numbers = numblist zero

numblist zero => ... =>
cons zero (numblist (succ zero)) => ... =>
λs.(s zero (numblist (succ zero)))

head numbers => ... =>
λs.(s zero (numblist (succ zero))) λx.λy.x => ... =>
zero

tail numbers => ... =>
λs.(s zero (numblist (succ zero))) λx.λy.y => ... =>
numblist (succ zero) => ... =>
λs.(s (succ zero) (numblist (succ (succ zero))))

Lazy evaluation: is a method of delaying expression evaluation which avoids multiple
evaluation of the same expression. Lazy evaluation is also known as call by need.

(λs.(s s)1 (λx.x λy.y)2)3

FUNCTIONAL PROGRAMMING IN STANDARD ML

Introduction

- <expression>;
> <result>

Types: types are central to SML. Every object and construct is typed.

Standard types: boolean, integers, strings, lists, tuples.

<value> : <type>
fn : <type>

Basic types - booleans, integers and strings

The type expression for a basic type is the type's identify.
The boolean type has identifier:
           bool
and values:
          true false
- true;
> true : bool

The integer type has identifier:
          int 
- 42;
> 42 : int
- ~84
> ~84 : int

The string type has identifier:
        string
- "Is this a string?";
> "Is this a string?" : string

List

empty list 
[]

The type expression for a list depends on the element type:

<element type> list

- [1,4,9,16];
> [1,4,9,16] : int list

- [[1,2],[3,4]]
> [[1,2],[3,4]] : (int list) list

Tuples

("ui",1,2)

A tuple's type is represented by its element's types separated by *s:
<element1 type> * <element2 type> * ...

- ("aa",1)
> ("aa",1) : string * int

6. Function types and expressions

fn : <domain type> -> <range type>

<function expression> <argument expression>

7. Boolean standard functions

- not true;
> false : bool

- not;
> fn : bool -> bool

- true orelse false;
> true : bool

- true andalso false;
> fasle: bool

fn : (bool * bool) -> bool

8. Numeric standard functions and operator overloading

- op div;
> fn : (int*int) -> int

9. String standard functions

- op ^;
> fn : (string * string) -> string

- "Happy"^" birthday!";
> "Happy birthday!" : string

- size;
> fn : string -> int

- size "hello";
> 5 : int

10. List standard functions

- hd;
> fn : ('a list) -> 'a

- hd [1,2,3,4,5];
> 1 : int

- tl;
> fn : ('a list) -> ('a list)

- tl ["a","b"]
> ["b"] : string list

- op ::;
> ('a * ('a list)) -> ('a list)

- 0::[1,2,3,4,5];
>[0,1,2,3,4,5] : int list

11. Characters,strings and lists

- ord;
> fn : string -> int

- chr;
> fn : int -> string

- chr 54;
> "6" : string

- explode;
> fn : string -> (string list)

- explode "hello";
> ["h","e","l","l","o"] : string list

- implode;
> fn: (string list) -> string

- implode ["Time ","for ", "tea?"]
> "Time for tea?" : string

12 Comparison operators

13 Function

fn <bound variables> => expression

- fn x => x + 1
> fn : int -> int

- fn x => fn y => not (x orelse y);
> fn : bool -> (bool -> bool)

- fn (x,y) => not (x orelse y);
> fn : (bool * bool) -> bool

14 make bound variables` types explicit

- fn (x:int) => x*x;
> fn : int -> int

- fn (x:int,y:int) => x*x+y*y;
> fn : (int*int) -> int

15 Definition

val <name> = <expression>

- val sq = fn (x:int) => x*x
> val sq = fn : int -> int

16 Conditional expression

if <expression1>
then <expression2>
else <expressions>

- val max = fn (x:int,y:int) => if x>y
                                then x
                                else y;
> val max = fn : (int * int) -> int

- val imp = fn (x,y) => if x
                        then y
                        else true;
> val imp = fn : (bool * bool) -> bool

17 Recursion and function definitions

- val rec length = fn (l:int list) => if l = []
                                        then 0
                                        else 1 + (length (tl l))
> val length = fn : (int list) -> int

fun <name> <bound variables> = <expression> ==
val rec <name> = fn <bound variables> => <expression>

- fun squarel (l:int list) =
   if l = []
   then []
   else ((hd l)*(hd l))::(squarel (tl l));
> fun squarel = fn : (int list) -> (int list)

- fun sinsert (s:string,l:string list) =
   if l = []
   then [s]
   else
    if s < (hd l)
    then s::l
    else (hd l)::(sinsert (s,(tl l)));
> val sinsert = fn : (string * (string list)) -> (string list)

9.18 Tuple selection

- fun tname (n:(string*string), d:string, p:int) = n;
> val tname = fn : ((string * string) * string * int) -> (string * string)

- fun tdept (n:(string*string), d:string, p:int) = d;
> val tdept = fn : ((string * string) * string * int) -> string

- fun tno (n:(string*string), d:string, p:int) = p;
> val tno = fn : ((string * string) * string * int) -> int

9.19 Pattern matching

- fun ihd ((h:int)::(t:int list)) = h;
> val ihd = fn : (int list) -> int

- fun itl ((h:int)::(t:int list)) = t;
> val itl = fn : (int list) -> (int list)

fun <name> <pattern1> = <expression1> |
    <name> <pattern2> = <expression2> |
    ...
    <name> <patternN> = <expressionN> |

- fun captital "Denmark" = "Copenhagen" |
      captital "Finland" = "Helsinki"   |
      captital "Norway"  = "Oslo"       |
      captital _         =  "not in cap";
> val captital = fn : string -> string

- fun length [] = 0 |
      length (_::(t:int list)) = 1 + (length t);
> val length = fn : (int list) -> int

- fun cubes 0 = [0] |
      cubes (n:int) = (n*n*n)::(cubes (n-1));
> val cubes = fn int -> (int list)

- fun sifind _ [] = "can't find it" |
      sifind 0 ((h:string)::_) = h  |
      sifind (i:int) (_::(t:string list)) = sifind (i-1) t;
> val sifind = fn : int -> ((string list) -> string)

9.20 Local definitions

SML uses the let ... in ... notatin for local definitions

    let val <name> = <expression1>
    in <expression2>
    end

    let fun <name> <pattern> = <expression1>
    in <expression2>
    end

- fun sort [] = [] |
      sort ((h:int)::(t:int list)) =
       let fun insert (i:int) [] = [i] |
               insert (i:int) ((h:int)::(t:int list)) =
                if i<h
                then i::h::t
                else h::(insert i t)
       in insert h (sort t)
       end
> val sort = fn : (int list) -> (int list)

21. Type expressions and abbreviated types

type <abbreviation> = <type expression>

- type forename = string;
> type forename = string

- type surname = string;
> type surname = string

- type person = forename * surname;
> type person = forename * surname

- type department = string;
> type department = string

- type extension = int;
> type extension = int

- type entry = person * department * extension;
> type entry = person * department * extension

22 Type variables and polymorphism

     - fun hd (h::t) = h;
     > val hd = fn : (`a list) -> `a
     
     - fun tl (h::t) = t;
     > val tl = fn : (`a list) -> (`a list)
     
     ::
     (`a * (`a list)) -> (`a list)
     
     - fun first (x,y,x) = x;
     > val first = fn : (`a * `b * `c) -> `a 
     
     - fun second (x,y,x) = y;
     > val second = fn : (`a * `b * `c) -> `b 
     
     - fun third (x,y,z) = z;
     > val third = fn : (`a * `b * `c) -> `z
     
     - fun length [] = 0 |
           length (h::t) = 1 + (length t);
     > val length = fn : (`a list) -> list
     
     - fun insert i [] = [i] |
           insert i (h::t) =
            if i<h
            then i::h::t
            else h::(insert i t);
     
     - fun insert _ i [] = [i] |
           insert comp i (h::t) =
            if comp (i,h)
            then i::h::t
            else h::(insert com i t); 
     > val insert = ((`a * `a) -> bool) -> (`a -> ((`a list) -> (`a list)))
     
     - val sinsert = insert (fn (s1:string,s2:string) => s1<2);
     > val sinsert = fn : string -> ((string, list) -> (string list))
     
     - val iinsert = insert (fn (i1:int,i2:int) => i1<i2);
     > val iinsert = fn : int -> ((int list) -> (int list))

23. New types

A new concrete type may be introduced by a datatype binding.

1. listing base values explicitly
2. defining structured values in terms of base values and other structured values.

      datatype <constructor> = <constructor1> |
                               <constructor2> |
                               ...
                               <constructorN>

      - datatype bool = true | false;
      > datatype bool = true | false;
        con true = true : bool
        con false = false : bool

      - datatype traffic_light = red | red_amber | green | amber;
      > datatype traffic_light = red | red_amber | green | amber
        con red = red : traffic_light
        con red_amber = red_amber : traffic_light
        con green = green : traffic_light
        con amber = amber : traffic_light

      - fun change red         = red_amber |
            change red_amber   = green     |
            change green       = amber     |
            change amber       = red;
      > val change = fn : traffic_light -> traffic_light

      datatype <constructor> = <constructor1> of <type expression1> |
                               <constructor2> of <type expression2> |
                               ...
                               <constructorN> of <type expressionN> |

      - datatype intlist = intnil | intcons of int * intlist;
      > datatype intlist = intnil | intcons of int * intlist
        con intnil = intnil : intlist
        con intcons = fn : (int * intlist) -> intlist

      - intcons(1,intnil);
      > intcons(1,intnil) : intlist
      
      - intcons(1,intcons(2,intnil));
      > intcons(1,intcons(2,intnil)) : intlist
      
      - intcons(1,intcons(2,intcons(3,intnil)));
      > intcons(1,intcons(2,intcons(3,intnil))) : intlist
      
      - datatype `a list = lnil | cons of `a * (`a list);
      > datatype `a list = lnil | cons of `a * (`a list)
        con lnil : `a list
        con cons  = fn : (`a * `a list) -> (`a list)
      
      - type intlist = int list
      > type intlist = int list
      
      - cons("ant",lnil);
      > cons("ant",lnil) : string list
      
      - fun sum intnil = 0 |
            sum (intcons(x:int, y:intlist)) = x + (sum y);
      - val sum = fn : intlist -> int
      
      - sum (intcons(9,intcons(8,intcons(7,intnil))));
      > 24 : int
      
      - fun join lnil = "" |
            join (cons(s:string,l:(string list))) = s^join l;
      > val join = fn : (string list) -> string
      
      - datatype number = int | real;
      > datatype number = int | real
        con int = int : number
        con real = int : number
      
      
      - datatype number = intnumb of int | realnumb of real;
      > datatype number = intnumb of int | realnumb of real
        con intnumb = fn : int -> number
        con realnumb = fn : real -> number
      
      - intnumb(1);
      > intnumb(1) : number
      
      - realnumb(1.1)
      > realnumb(1.1) : number
      
      - fun ivalue (intnumb(n:int)) = n
      > val ivalue = fn : number -> int
      
      - fun rvalue (realnumb(n: real)) = n
      > val rvalue = fn : number -> real
      
      - ivalue (intnumb(1));
      > 3 : int
      
      - rvalue (realnumb(1.1));
      > 1.1 : real

24. Tree

     - datatype inttree = empty | node of int * inttree * inttree
     > datatype inttree = empty | node of int * inttree * inttree
       con empty = empty : inttree
       con node = fn : (int * inttree * inttree) -> inttree
    
    - fun add (v:int) empty = node(v,empty,empty) |
          add (v:int) (node(nv:int,l:inttree,r:inttree)) =
           if v < nv
           then node(nv,add v l,r)
           else node(nv,l,add v r);
    > val add = fn : int -> (inttree -> inttree)
    
    - fun traverse empty = [] |
          traverse (node(v:int,l:inttree,r:inttree)) =
           append (traverse l) (v::traverse r);
    > val traverse = fn : inttree -> (int list)
    
    - traverse root;
    > [2,3,4,5,7,9] : int list
    
    - datatype `a tree = empty | node of `a * (`a tree) * (`a tree);
    > datatype `a tree = empty | node of `a * (`a tree) * (`a tree)
      con empty = empty : (`a tree)
      con ndoe = fn : (`a * (`a tree) (`a tree)) -> (`a tree)
      
    - fun add _ (v:'a) empty = node(v,empty,empty) |
          add (less:'a -> ('a tree))
              (v:'a)
              (node(nv:'a,l:'a tree,r:'a tree)) =
            if less v nv
            then node(nv,add less v l,r)
            else node(nv,l,add less v r);
    > val add = fn : ('a -> ('a -> bool)) ->
                     ('a -> (('a tree) -> ('a tree)))
    
    - fun traverse empty = [] |
          traverse (node(v:'a,l:'a tree,'a tree)) =
           append (traverse l) (v::traverse r);
    > val traverse = fn : ('a tree) -> ('a list)

25 lambda calculus in ML