import unittest

from app.lambda_calculus import identify, self_apply, apply, identify2
from app.lambda_calculus import select_first, select_second, make_pair


class TestLambdaCulculus(unittest.TestCase):

    # def setUp(self):

    def test_identify(self):
        self.assertEqual(1, identify()(1))
        self.assertEqual(1, identify2()(1))
        self.assertEqual(identify, identify()(identify))
    
    def test_self_apply(self):
        self.assertEqual(self_apply, identify()(self_apply))
    
    # def test_apply(self):
        # self.assertEqual(1, apply()(identify())(self_apply()))

    def test_select_first(self):
        self.assertEqual(1, select_first()(1)(2))
        self.assertEqual(2, select_second()(1)(2))

    def test_make_pair(self):
        self.assertEqual(1, make_pair()(1)(2)(select_first()))
        self.assertEqual(2, make_pair()(1)(2)(select_second()))