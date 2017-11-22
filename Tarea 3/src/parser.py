from collections import namedtuple
from numbers import Number

from pyfpm.matcher import Matcher, match_args
from sexpdata import loads, Symbol

from Exceptions import WrongSyntaxException

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
        print(arg_list)

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


