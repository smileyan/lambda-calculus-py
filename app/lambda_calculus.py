# lambda calculus was devised by Alonzo Church in the 1930's

# lambda expressions: 
# <expression> ::= <name> | <function> | <application>
# lambda function:
# <function> ::= lambda<name>.<body>
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

def select_first():
    return lambda first: lambda second: first

def select_second():
    return lambda first: lambda second: second

def make_pair():
    return lambda first: lambda second: lambda func: func(first)(second)