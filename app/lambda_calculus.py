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
error_type = one

MAKE_ERROR = make_obj(error_type)

ERROR = MAKE_ERROR(error_type)

def is_error(obj):
    return istype(error_type)(obj)