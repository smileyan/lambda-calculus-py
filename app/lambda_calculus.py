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

def identify():
    return lambda x: x

def self_apply():
    return lambda s: s(s)

def apply():
    return lambda func: lambda arg: func(arg)

def identify2():
    return lambda x: apply()(identify())(x)

def _select_first_():
    return lambda first: lambda second: first

select_first = _select_first_()

def _select_second_():
    return lambda first: lambda second: second

select_second = _select_second_()

def make_pair():
    return lambda first: lambda second: lambda func: func(first)(second)

# Truth values and conditional expression
#     <condition>?<expression>:<expression>

def _cond_():
    return lambda e1: lambda e2: lambda c: c(e1)(e2)
cond = _cond_()

def _true():
    return select_first

true = _true()

def _false():
    return select_second

false = _false()

# NOT <operand>

def _not_():
    # return lambda x: cond()(_false())(_true())(x)
    return lambda x: x(false)(true)

_not = _not_()

# AND <operand> AND <operand>

def _and_():
    # return lambda x: lambda y: cond()(y)(_false())(x)
    return lambda x: lambda y: x(y)(false)

_and = _and_()

# OR <operand> OR <operand>
def _or_():
    # return lambda x: lambda y: cond(true)(y)(x)
    return lambda x: lambda y: x(true)(y)

_or = _or_()

def _zero_():
    return identify()

zero = _zero_()

# succ = λn.λs((s false) n)
def _succ_():
    return lambda n: lambda s: s(false)(n)

succ = _succ_()

one = succ(zero)

# one ==
# (succ zero) == 
# (λn.λs.((s false) n) zero) =>
# λs.((s false) zero)

two = succ(succ(zero))

# two ==
# (succ one) ==
# (λn.λs.((s false) n) one) =>
# λs.((s false) one) ==
# λs.((s false) λs.((s false) zero))

three = succ(two)

def _iszero_():
    return lambda n: n(select_first)

iszero = _iszero_()