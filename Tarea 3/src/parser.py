from collections import namedtuple
from numbers import Number

from pyfpm.matcher import Matcher
from sexpdata import loads, Symbol

from src.Exceptions import WrongSyntaxException

Num = namedtuple('num', 'n')
Id = namedtuple('id', 's')
Add = namedtuple('add', ['l', 'r'])
Sub = namedtuple('sub', ['l', 'r'])
IF0 = namedtuple('if0', ['c', 't', 'f'])
App = namedtuple('app', ['fun', 'arg'])
Fun = namedtuple('fun', ['id', 'body'])
Seqn = namedtuple('seqn', ['exp1', 'exp2'])
Set = namedtuple('set', ['id', 'exp'])


def parse(s_expr):
    def parser(arg_list):

        if isinstance(arg_list, Number):
            return Num(arg_list)
        if isinstance(arg_list, Symbol):
            return Id(arg_list.value())

        n_args = len(arg_list)
        op = arg_list[0].value()

        if n_args == 1:
            if isinstance(arg_list[0], Symbol):
                return Id(arg_list[0].value())
        if n_args == 2:
            return App(parser(arg_list[0]), parser(arg_list[1]))
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


NumV = namedtuple('numV', ['n'])
ClosureV = namedtuple('closureV', ['id', 'body', 'env'])

MTEnv = namedtuple('mtEnv', [])
AEnv = namedtuple('aEnv', ['id', 'loc', 'env'])


def env_lookup(x, env):
    match = Matcher([
        ('MTEnv()', lambda: 'identificador libre'),
        ('AEnv(iid, loc, rest)', lambda iid, loc, rest: loc if iid == x else env_lookup(x, rest)),
    ])
    return match(env)


def interp(expr, env):
    def num_plus(n1, n2):
        return NumV(n1.n + n2.n)

    def sub_min(n1, n2):
        return NumV(n1.n - n2.n)

    def check_if(c, t, f):
        if c.n == 0:
            return t
        return f

    def check_app(exp, arg):
        closure = interp(exp, env)
        new_env = AEnv(closure.id, interp(arg, env), closure.env)
        return interp(closure.body, new_env)

    match = Matcher([
        ('Num(n)', lambda n: NumV(n)),
        ('Add(l, r)', lambda l, r: num_plus(interp(l, env), interp(r, env))),
        ('Sub(l, r)', lambda l, r: sub_min(interp(l, env), interp(r, env))),
        ('Fun(iid, body)', lambda iid, body: ClosureV(iid, body, env)),
        ('IF0(c, t, f)', lambda c, t, f: check_if(interp(c, env), interp(t, env), interp(f, env))),
        ('Id(s)', lambda s: env_lookup(s, env)),
        ('App(exp, arg)', lambda exp, arg: check_app(exp, arg)),
    ])

    return match(expr)


print(interp(parse("(with (x 3) (with (f (fun (y) (+ x y))) (with (x 5) (f 4))))"), MTEnv()))


a = Num(10)
b = Add(Num(10),a)

print(b)
a = a._replace(n=20)
print(b)
