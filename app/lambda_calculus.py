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
cond = lambda e1: lambda e2: lambda c: c(lambda : e1)(lambda : e2)

true = select_first

false = select_second

# NOT <operand>
_not = lambda x: x(false)(true)

# AND <operand> AND <operand>
_and = lambda x: lambda y: x(y)(false)

# OR <operand> OR <operand>
_or = lambda x: lambda y: x(true)(y)

zero = identify

# succ = λn.λs((s false) n)
def _succ_():
    return lambda n: lambda s: s(false)(n)

succ = _succ_()

one = succ(zero)

# one ==
# (succ zero) == 
# (λn.λs.((s false) n) zero) =>
# λs.((s false) zero)

two = succ(one)

# two ==
# (succ one) ==
# (λn.λs.((s false) n) one) =>
# λs.((s false) one) ==
# λs.((s false) λs.((s false) zero))

three = succ(two)

# three ==
# (succ two) ==
# (λn.λs((s false) n) two) ==
# (λn.λs((s false) n) λs.((s false) λs.((s false) zero)))

# λs.((s false) <number>)
# (λs.((s false) <number>) select_first) =>
# ((select_first false) <number>) ==
# ((λfirst.λsecond.first false) <number>) =>
# (λsecond.false <number>) =>
# false

# (zero select_first) ==
# (λx.x select_first) =>
# select_first ==
# true
def _iszero_():
    return lambda n: n(select_first)

iszero = _iszero_()

# (pred one) => ... => zero
# (pred two) => ... => one
# (pred three) => ... => two

# (λs.((s false) <number>) select_second) =>
# ((select_second false) <number) ==
# ((λfirst.λsecond.second false) <number>) =>
# (λsecond.second <number>) =>
# <number>

def _pred():
    return lambda n: iszero(n)(zero)(n(select_second))
pred = _pred()

Y = lambda f: (lambda x: x(x))(lambda y: f(lambda *args: y(y)(*args)))

def add1():
    return lambda f: lambda x: lambda y: cond(x)(f(succ(x)(pred(y))))

def add():
    return Y(add1())