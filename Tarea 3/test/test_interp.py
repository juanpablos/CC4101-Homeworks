from unittest import TestCase

from src.language import interp, Num, Id, Add, Sub, Fun, App, IF0, Seqn, Set, MTEnv, NumV, ClosureV


class TestInterp(TestCase):
    def test_parse_num(self):
        expr = interp(Num(4), MTEnv())
        expected = NumV(4)
        self.assertEqual(expr.val, expected)

    def test_parse_id(self):
        try:
            interp(Id('x'), MTEnv())
            self.fail()
        except:
            pass

    def test_parse_sum(self):
        expr = interp(Add(Num(10), Num(4)), MTEnv())
        expected = NumV(14)
        self.assertEqual(expr.val, expected)

    def test_parse_sub(self):
        expr = interp(Sub(Num(15), Num(10)), MTEnv())
        expected = NumV(5)
        self.assertEqual(expr.val, expected)

    def test_parse_with(self):
        expr = interp(App(Fun('x', Add(Id('x'), Num(3))), Num(7)), MTEnv())
        expected = NumV(10)
        self.assertEqual(expr.val, expected)

    def test_parse_if(self):
        expr = interp(IF0(Sub(Num(7), Num(7)), Num(4), Num(10)), MTEnv())
        expected = NumV(4)
        self.assertEqual(expr.val, expected)

    def test_parse_fun(self):
        expr = interp(Fun('x', Sub(Num(10), Id('x'))), MTEnv())
        expected = ClosureV('x', Sub(Num(10), Id('x')), MTEnv())
        self.assertEqual(expr.val, expected)

    def test_parse_app(self):
        expr = interp(App(Fun('x', Add(Num(1), Id('x'))), Num(4)), MTEnv())
        expected = NumV(5)
        self.assertEqual(expr.val, expected)

    def test_parse_enun_fun(self):
        expr = interp(Fun('x', Id('x')), MTEnv())
        expected = ClosureV('x', Id('x'), MTEnv())
        self.assertEqual(expr.val, expected)

    def test_parse_inval(self):
        try:
            interp(App(Num(8), Num(10)), MTEnv())
            self.fail()
        except:
            pass

    def test_parse_enun_fun2(self):
        expr = interp(App(App(Fun('x', Id('x')), Fun('x', Add(Id('x'), Num(5)))), Num(3)), MTEnv())
        expected = NumV(8)
        self.assertEqual(expr.val, expected)

    def test_parse_enun_with(self):
        expr = interp(
            App(Fun('x', App(Fun('f', App(Fun('x', App(Id('f'), Num(4))), Num(5))), Fun('y', Add(Id('x'), Id('y'))))),
                Num(3)), MTEnv())
        expected = NumV(7)
        self.assertEqual(expr.val, expected)

    def test_parse_enun_setseqn(self):
        expr = interp(App(Fun('x', Add(Seqn(Set(Id('x'), Num(5)), Id('x')), Id('x'))), Num(3)), MTEnv())
        expected = NumV(10)
        self.assertEqual(expr.val, expected)
