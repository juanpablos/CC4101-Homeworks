from unittest import TestCase

from src.language import error, MTEnv, env_lookup, AEnv, Num, MTSto, ASto, sto_lookup, loc_lookup, next_loc


class TestAux(TestCase):
    def test_error(self):
        try:
            error('x')
            self.fail()
        except:
            pass

    def test_look_env(self):
        env = MTEnv()
        try:
            env_lookup('x', env)
            self.fail()
        except:
            pass

    def test_look_env2(self):
        env = AEnv('x', Num(3), MTEnv())
        iid = env_lookup('x', env)
        expected = Num(3)
        self.assertEqual(iid, expected)

    def test_look_sto(self):
        sto = MTSto()
        try:
            sto_lookup(1, sto)
            self.fail()
        except:
            pass

    def test_look_sto2(self):
        sto = ASto(2, Num(3), MTSto())
        iid = sto_lookup(2, sto)
        expected = Num(3)
        self.assertEqual(iid, expected)

    def test_look_loc(self):
        sto = MTSto()
        try:
            loc_lookup(Num(10), sto)
            self.fail()
        except:
            pass

    def test_look_loc2(self):
        sto = ASto(2, Num(3), MTSto())
        iid = loc_lookup(Num(3), sto)
        expected = 2
        self.assertEqual(iid, expected)

    def test_next_loc(self):
        sto = MTSto()
        real = next_loc(sto)
        expected = 0
        self.assertEqual(real, expected)

    def test_next_loc2(self):
        sto = ASto(2, Num(3), MTSto())
        real = next_loc(sto)
        expected = 1
        self.assertEqual(real, expected)
