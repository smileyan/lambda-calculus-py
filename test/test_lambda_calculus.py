import unittest

from app.lambda_calculus import identify, self_apply, apply
from app.lambda_calculus import select_first, select_second, make_pair
from app.lambda_calculus import cond, true, false, _not, _and, _or
from app.lambda_calculus import zero, succ, one, two, three, iszero, pred
from app.lambda_calculus import add, mul, sub, fact, equal, greater
from app.lambda_calculus import add_y, fact_y

class TestLambdaCulculus(unittest.TestCase):

    # def setUp(self):

    def test_identify(self):
        self.assertEqual(1, identify(1))
        self.assertEqual(identify, identify(identify))
    
    def test_self_apply(self):
        self.assertEqual(self_apply, identify(self_apply))
    
    # def test_apply(self):
        # self.assertEqual(1, apply()(identify())(self_apply()))

    def test_select_first(self):
        self.assertEqual(1, select_first(1)(2))
        self.assertEqual(2, select_second(1)(2))

    def test_make_pair(self):
        self.assertEqual(1, make_pair(1)(2)(select_first))
        self.assertEqual(2, make_pair(1)(2)(select_second))

    def test_cond(self):
        self.assertEqual(1, cond(lambda :1)(lambda :2)(true))
        self.assertEqual(2, cond(lambda :1)(lambda :2)(false))
    
    def test_not(self):
        self.assertEqual(1, cond(lambda :1)(lambda :2)(_not(false)))
        self.assertEqual(2, cond(lambda :1)(lambda :2)(_not(true)))

    def test_and(self):
        self.assertEqual(true, _and(true)(true))
        self.assertEqual(false, _and(false)(false))
        self.assertEqual(false, _and(true)(false))
        self.assertEqual(false, _and(false)(true))
        self.assertEqual(1, cond(lambda :1)(lambda :2)(_and(true)(true)))
        self.assertEqual(2, cond(lambda :1)(lambda :2)(_and(false)(true)))
    
    def test_or(self):
        self.assertEqual(true, _or(false)(true))
        self.assertEqual(true, _or(true)(true))
        self.assertEqual(false, _or(false)(false))
        self.assertEqual(true, _or(true)(true))

    def test_numbers(self):
        self.assertEqual(identify, zero)
        self.assertEqual(true, iszero(zero))
        self.assertEqual(false, iszero(one))
        self.assertEqual(zero, pred(one))
        self.assertEqual(one, pred(two))

    def test_fact(self):
        self.assertEqual(120, fact(5))
        self.assertEqual(120, fact_y(5))
        self.assertEqual(30, add(10)(20))
        self.assertEqual(30, add_y(10)(20))
        self.assertEqual(200, mul(10)(20))
        self.assertEqual(-1, sub(1)(2))
        self.assertEqual(True, equal(3)(3))
        self.assertEqual(False, equal(1)(2))
        self.assertEqual(True, greater(3)(1))
        self.assertEqual(False, greater(1)(2))
