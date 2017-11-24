from unittest import TestCase

from src.language import parse, Num, Id, Add, Sub, Fun, App, IF0, Seqn, Set


class TestParse(TestCase):
    def test_parse_num(self):
        expr = parse("4")
        expected = Num(4)
        self.assertEqual(expr, expected)

    def test_parse_id(self):
        expr = parse("x")
        expected = Id('x')
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
        expected = App(Fun('x', Add(Num(1), Id('x'))), Num(4))
        self.assertEqual(expr, expected)

    def test_parse_enun_fun(self):
        expr = parse("(fun (x) x)")
        expected = Fun('x', Id('x'))
        self.assertEqual(expr, expected)

    def test_parse_inval(self):
        expr = parse("(8 10)")
        expected = App(Num(8), Num(10))
        self.assertEqual(expr, expected)

    def test_parse_enun_fun2(self):
        expr = parse("(((fun (x) x) (fun (x) (+ x 5))) 3)")
        expected = App(App(Fun('x', Id('x')), Fun('x', Add(Id('x'), Num(5)))), Num(3))
        self.assertEqual(expr, expected)

    def test_parse_enun_with(self):
        expr = parse("(with (x 3) (with (f (fun (y) (+ x y))) (with (x 5) (f 4))))")
        expected = App(
            Fun('x', App(Fun('f', App(Fun('x', App(Id('f'), Num(4))), Num(5))), Fun('y', Add(Id('x'), Id('y'))))),
            Num(3))
        self.assertEqual(expr, expected)

    def test_parse_enun_setseqn(self):
        expr = parse("(with (x 3) (+ (seqn (set x 5) x) x))")
        expected = App(Fun('x', Add(Seqn(Set(Id('x'), Num(5)), Id('x')), Id('x'))), Num(3))
        self.assertEqual(expr, expected)
