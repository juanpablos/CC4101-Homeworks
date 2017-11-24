import sys
from io import StringIO
from unittest import TestCase

from src.language import run


class TestRun(TestCase):
    def test_parse_num(self):
        expr = run("4")
        expected = '4'
        self.assertEqual(expr, expected)

    def test_parse_id(self):
        with self.assertRaises(SystemExit):
            capturedOutput = StringIO()
            sys.stdout = capturedOutput
            run("x")
            sys.stdout = sys.__stdout__
        expected = 'error: identificador libre!! x\n'
        self.assertEqual(capturedOutput.getvalue(), expected)

    def test_parse_sum(self):
        expr = run("(+ 10 4)")
        expected = '14'
        self.assertEqual(expr, expected)

    def test_parse_sub(self):
        expr = run("(- 15 10)")
        expected = '5'
        self.assertEqual(expr, expected)

    def test_parse_with(self):
        expr = run("(with (x 7) (+ x 3))")
        expected = '10'
        self.assertEqual(expr, expected)

    def test_parse_if(self):
        expr = run("(if0 (- 7 7) 4 10)")
        expected = '4'
        self.assertEqual(expr, expected)

    def test_parse_fun(self):
        expr = run("(fun (x) (- 10 x))")
        expected = 'Function'
        self.assertEqual(expr, expected)

    def test_parse_app(self):
        expr = run("((fun (x) (+ 1 x)) 4)")
        expected = '5'
        self.assertEqual(expr, expected)

    def test_parse_enun_fun(self):
        expr = run("(fun (x) x)")
        expected = 'Function'
        self.assertEqual(expr, expected)

    def test_parse_inval(self):
        with self.assertRaises(SystemExit):
            capturedOutput = StringIO()
            sys.stdout = capturedOutput
            run("(8 10)")
            sys.stdout = sys.__stdout__
        expected = 'error: expresión inválida (8 10)\n'
        self.assertEqual(capturedOutput.getvalue(), expected)

    def test_parse_enun_fun2(self):
        expr = run("(((fun (x) x) (fun (x) (+ x 5))) 3)")
        expected = '8'
        self.assertEqual(expr, expected)

    def test_parse_enun_with(self):
        expr = run("(with (x 3) (with (f (fun (y) (+ x y))) (with (x 5) (f 4))))")
        expected = '7'
        self.assertEqual(expr, expected)

    def test_parse_enun_setseqn(self):
        expr = run("(with (x 3) (+ (seqn (set x 5) x) x))")
        expected = '10'
        self.assertEqual(expr, expected)
