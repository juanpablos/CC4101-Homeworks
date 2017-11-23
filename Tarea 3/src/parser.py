import sys
from collections import namedtuple
from numbers import Number

from pyfpm.matcher import Matcher
from sexpdata import loads, Symbol

from src.Exceptions import WrongSyntaxException, FreeIdentifier

Num = namedtuple('num', 'n')
Id = namedtuple('id', 's')
Add = namedtuple('add', ['l', 'r'])
Sub = namedtuple('sub', ['l', 'r'])
IF0 = namedtuple('if0', ['c', 't', 'f'])
App = namedtuple('app', ['fun', 'arg'])
Fun = namedtuple('fun', ['id', 'body'])
Seqn = namedtuple('seqn', ['exp1', 'exp2'])
Set = namedtuple('set', ['id', 'exp'])

NumV = namedtuple('numV', ['n'])
ClosureV = namedtuple('closureV', ['id', 'body', 'env'])

MTEnv = namedtuple('mtEnv', [])
AEnv = namedtuple('aEnv', ['id', 'loc', 'env'])

MTSto = namedtuple('mtSto', [])
ASto = namedtuple('aSto', ['loc', 'val', 'sto'])

ValStore = namedtuple('v_s', ['val', 'sto'])


def parse(s_expr):
    def parser(arg_list):

        if isinstance(arg_list, Number):
            return Num(arg_list)
        if isinstance(arg_list, Symbol):
            return Id(arg_list.value())

        n_args = len(arg_list)

        if n_args == 1:
            if isinstance(arg_list[0], Symbol):
                return Id(arg_list[0].value())
        if n_args == 2:
            return App(parser(arg_list[0]), parser(arg_list[1]))

        op = arg_list[0].value()
        if n_args == 3:
            if op == '+':
                return Add(parser(arg_list[1]), parser(arg_list[2]))
            if op == '-':
                return Sub(parser(arg_list[1]), parser(arg_list[2]))
            if op == 'fun':
                return Fun(arg_list[1][0].value(), parser(arg_list[2]))
            if op == 'with':
                return App(Fun(arg_list[1][0].value(), parser(arg_list[2])), parser(arg_list[1][1]))
            if op == 'seqn':
                return Seqn(parser(arg_list[1]), parser(arg_list[2]))
            if op == 'set':
                return Set(parser(arg_list[1]), parser(arg_list[2]))
        if n_args == 4:
            if op == 'if0':
                return IF0(parser(arg_list[1]), parser(arg_list[2]), parser(arg_list[3]))

        raise WrongSyntaxException("Wrong syntax in {}".format(s_expr))

    try:
        s_expr_list = loads(s_expr)
        return parser(s_expr_list)
    except WrongSyntaxException:
        raise
    except:
        raise WrongSyntaxException("Wrong syntax in {}".format(s_expr))


def error(identifier):
    raise FreeIdentifier(identifier)


def env_lookup(x, env):
    match = Matcher([
        ('MTEnv()', lambda: error(x)),
        ('AEnv(iid, loc, rest)', lambda iid, loc, rest: loc if iid == x else env_lookup(x, rest)),
    ])
    return match(env)


def sto_lookup(l, sto):
    match = Matcher([
        ('MTSto()', lambda: error(l)),
        ('ASto(loc, val, rest)', lambda loc, val, rest: val if loc == l else sto_lookup(l, rest)),
    ])
    return match(sto)


def loc_lookup(val, sto):
    match = Matcher([
        ('MTSto()', lambda: error(val)),
        ('ASto(loc, s_val, rest)', lambda loc, s_val, rest: loc if s_val == val else loc_lookup(val, rest)),
    ])
    return match(sto)


def next_loc(sto):
    match = Matcher([
        ('MTSto()', lambda: 0),
        ('ASto(_, _, rest)', lambda rest: 1 + next_loc(rest)),
    ])
    return match(sto)


def interp(expr, env, sto):
    def num_plus(n1, n2):
        return NumV(n1.n + n2.n)

    def num_sub(n1, n2):
        return NumV(n1.n - n2.n)

    def do_sum(l, r, s_env, s_sto):
        l_vs = interp(l, s_env, s_sto)
        r_vs = interp(r, s_env, l_vs.sto)
        return ValStore(num_plus(l_vs.val, r_vs.val), r_vs.sto)

    def do_sub(l, r, s_env, s_sto):
        l_vs = interp(l, s_env, s_sto)
        r_vs = interp(r, s_env, l_vs.sto)
        return ValStore(num_sub(l_vs.val, r_vs.val), r_vs.sto)

    def do_if(c, t, f, s_env, s_sto):
        c_vs = interp(c, s_env, s_sto)
        if c_vs.val.n == 0:
            return interp(t, s_env, c_vs.sto)
        else:
            return interp(f, s_env, c_vs.sto)

    def do_app(f_exp, arg, s_env, s_sto):
        f_vs = interp(f_exp, s_env, s_sto)
        a_vs = interp(arg, s_env, f_vs.sto)
        new_loc = next_loc(a_vs.sto)
        return interp(f_vs.val.body,
                      AEnv(f_vs.val.id, new_loc, f_vs.val.env),
                      ASto(new_loc, a_vs.val, a_vs.sto))

    def do_seq(exp1, exp2, s_env, s_sto):
        vs_1 = interp(exp1, s_env, s_sto)
        return interp(exp2, s_env, vs_1.sto)

    def do_set(the_id, exp, s_env, s_sto):
        i_vs = interp(the_id, s_env, s_sto)
        loc = loc_lookup(i_vs.val, s_sto)
        e_vs = interp(exp, s_env, i_vs.sto)
        return ValStore(e_vs.val, ASto(loc, e_vs.val, e_vs.sto))

    match = Matcher([
        ('Num(n)', lambda n: ValStore(NumV(n), sto)),
        ('Add(l, r)', lambda l, r: do_sum(l, r, env, sto)),
        ('Sub(l, r)', lambda l, r: do_sub(l, r, env, sto)),
        ('Fun(iid, body)', lambda iid, body: ValStore(ClosureV(iid, body, env), sto)),
        ('IF0(c, t, f)', lambda c, t, f: do_if(c, t, f, env, sto)),
        ('Id(s)', lambda s: ValStore(sto_lookup(env_lookup(s, env), sto), sto)),
        ('App(f_exp, arg)', lambda f_exp, arg: do_app(f_exp, arg, env, sto)),
        ('Seqn(exp1, exp2)', lambda exp1, exp2: do_seq(exp1, exp2, env, sto)),
        ('Set(iid, exp)', lambda iid, exp: do_set(iid, exp, env, sto)),
    ])

    try:
        return match(expr)
    except:
        raise


def run(prog):
    try:
        res = interp(parse(prog), MTEnv(), MTSto())
        match = Matcher([
            ('NumV(n)', lambda n: n),
            ('ClosureV(_, _, _)', lambda: 'Function'),
        ])

        return match(res.val)

    except FreeIdentifier as e:
        print("error: identificador libre!! {}".format(e))
    except Exception as e:
        print("error: expresión inválida {}".format(prog))
        print(e)
    exit(1)


if __name__ == "__main__":
    run(sys.argv[1])
