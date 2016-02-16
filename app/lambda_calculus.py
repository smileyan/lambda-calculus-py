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



