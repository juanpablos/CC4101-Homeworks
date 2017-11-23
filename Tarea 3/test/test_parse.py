from unittest import TestCase
from src.parser import parse, Num, Add, App, Fun, Id, Sub, IF0


class TestParse(TestCase):
    def test_parse_num(self):
        expr = parse("4")
        expected = Num(4)
        self.assertEqual(expr, expected)

    def test_parse_sum(self):
        expr = parse("(+ 10 4)")
        expected = Add(Num(10), Num(4))
        self.assertEqual(expr, expected)

    def test_parse_sub(self):
        expr = parse("(- 15 10)")
        expected = Sub(Num(15), Num(10))
        self.assertEqual(expr, expected)

    def test_parse_with(self):
        expr = parse("(with (x 7) (+ x 3))")
        expected = App(Fun('x', Add(Id('x'), Num(3))), Num(7))
        self.assertEqual(expr, expected)

    def test_parse_if(self):
        expr = parse("(if0 (- 7 7) 4 10)")
        expected = IF0(Sub(Num(7), Num(7)), Num(4), Num(10))
        self.assertEqual(expr, expected)

    def test_parse_fun(self):
        expr = parse("(fun (x) (- 10 x))")
        expected = Fun('x', Sub(Num(10), Id('x')))
        self.assertEqual(expr, expected)

    def test_parse_app(self):
        expr = parse("((fun (x) (+ 1 x)) 4)")
        expected = App((Fun('x', Add(Num(1), Id('x'))), Num(4)))
        self.assertEqual(expr, expected)
