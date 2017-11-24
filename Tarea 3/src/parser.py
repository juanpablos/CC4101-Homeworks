from collections import namedtuple
from numbers import Number

from pyfpm.matcher import Matcher
from sexpdata import loads, Symbol

from src.Exceptions import WrongSyntaxException, FreeIdentifier

"""
Expr

<s-expr> ::= <num>
         | (+ <s-expr> <s-expr>)
         | (- <s-expr> <s-expr>)
         | (if0 <s-expr> <s-expr> <s-expr>)
         | (with (<id> <s-expr>) <s-expr>)
         | <id>
         | (fun (<id>) <s-expr>)
         | (<s-expr> <s-expr>)
         | (seqn <s-expr> <s-expr>)
         | (set <id> <s-expr>)
"""
Num = namedtuple('num', 'n')
Id = namedtuple('id', 's')
Add = namedtuple('add', ['l', 'r'])
Sub = namedtuple('sub', ['l', 'r'])
IF0 = namedtuple('if0', ['c', 't', 'f'])
App = namedtuple('app', ['fun', 'arg'])
Fun = namedtuple('fun', ['id', 'body'])
Seqn = namedtuple('seqn', ['exp1', 'exp2'])
Set = namedtuple('set', ['id', 'exp'])

"""
Env

<env> ::= mtEnv()
        | aEnv(<id>, <loc>, <env>)
"""
MTEnv = namedtuple('mtEnv', [])
AEnv = namedtuple('aEnv', ['id', 'loc', 'env'])

"""
Values

<val> ::= NumV(<num>)
        | ClosureV(<id>, <expr>, <env>)
"""
NumV = namedtuple('numV', ['n'])
ClosureV = namedtuple('closureV', ['id', 'body', 'env'])

"""
Sto

<sto> ::= mtSto()
        | aSto(<loc>, <val>, <sto>)
<loc> ::= number
"""
MTSto = namedtuple('mtSto', [])
ASto = namedtuple('aSto', ['loc', 'val', 'sto'])

"""
Val*Sto

<vs> ::= ValStore(<val>, <sto>)
"""
ValStore = namedtuple('v_s', ['val', 'sto'])


def parse(s_expr):
    """
    parser :: String -> Expr

    Parses a string representing an s-expr and transforms it to ADT.

    where String is represented by the following:
    <s-expr> ::= <num>
             | (+ <s-expr> <s-expr>)
             | (- <s-expr> <s-expr>)
             | (if0 <s-expr> <s-expr> <s-expr>)
             | (with (<id> <s-expr>) <s-expr>)
             | <id>
             | (fun (<id>) <s-expr>)
             | (<s-expr> <s-expr>)
             | (seqn <s-expr> <s-expr>)
             | (set <id> <s-expr>)
    """

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
    """
    error :: <id> -> Exception
    Just matches a FreeIdentifier exception with an identifier.
    """
    raise FreeIdentifier(identifier)


def env_lookup(x, env):
    """
    env-lookup :: Sym Env -> Loc
    Searches for a symbol x in an environment and returns it's location id.
    Raises an error if not found.
    """
    match = Matcher([
        ('MTEnv()', lambda: error(x)),
        ('AEnv(iid, loc, rest)', lambda iid, loc, rest: loc if iid == x else env_lookup(x, rest)),
    ])
    return match(env)


def sto_lookup(l, sto):
    """
    sto-lookup :: Loc Sto -> Val
    Searches for a location l in a storage and returns it's associated value.
    Raises an error if not found.
    """
    match = Matcher([
        ('MTSto()', lambda: error(l)),
        ('ASto(loc, val, rest)', lambda loc, val, rest: val if loc == l else sto_lookup(l, rest)),
    ])
    return match(sto)


def loc_lookup(val, sto):
    """
    sto-lookup :: Val Sto -> Loc
    Searches for a value val in a storage and returns it's associated location id.
    Raises an error if not found.
    """
    match = Matcher([
        ('MTSto()', lambda: error(val)),
        ('ASto(loc, s_val, rest)', lambda loc, s_val, rest: loc if s_val == val else loc_lookup(val, rest)),
    ])
    return match(sto)


def next_loc(sto):
    """
    next_loc :: Sto -> Int
    Calculates the next available location id in a storage.
    """
    match = Matcher([
        ('MTSto()', lambda: 0),
        ('ASto(_, _, rest)', lambda rest: 1 + next_loc(rest)),
    ])
    return match(sto)


def interp(expr, env):
    """
    interp :: Expr x Env -> Val
    Evaluates a program withing an environment and returns the result of the evaluation.
    """

    def interp_rec(r_expr, r_env, sto):

        def num_plus(n1, n2):
            return NumV(n1.n + n2.n)

        def num_sub(n1, n2):
            return NumV(n1.n - n2.n)

        def do_sum(l, r, s_env, s_sto):
            l_vs = interp_rec(l, s_env, s_sto)
            r_vs = interp_rec(r, s_env, l_vs.sto)
            return ValStore(num_plus(l_vs.val, r_vs.val), r_vs.sto)

        def do_sub(l, r, s_env, s_sto):
            l_vs = interp_rec(l, s_env, s_sto)
            r_vs = interp_rec(r, s_env, l_vs.sto)
            return ValStore(num_sub(l_vs.val, r_vs.val), r_vs.sto)

        def do_if(c, t, f, s_env, s_sto):
            c_vs = interp_rec(c, s_env, s_sto)
            if c_vs.val.n == 0:
                return interp_rec(t, s_env, c_vs.sto)
            else:
                return interp_rec(f, s_env, c_vs.sto)

        def do_app(f_exp, arg, s_env, s_sto):
            f_vs = interp_rec(f_exp, s_env, s_sto)
            a_vs = interp_rec(arg, s_env, f_vs.sto)
            new_loc = next_loc(a_vs.sto)
            return interp_rec(f_vs.val.body,
                              AEnv(f_vs.val.id, new_loc, f_vs.val.env),
                              ASto(new_loc, a_vs.val, a_vs.sto))

        def do_seq(exp1, exp2, s_env, s_sto):
            vs_1 = interp_rec(exp1, s_env, s_sto)
            return interp_rec(exp2, s_env, vs_1.sto)

        def do_set(the_id, exp, s_env, s_sto):
            i_vs = interp_rec(the_id, s_env, s_sto)
            loc = loc_lookup(i_vs.val, s_sto)
            e_vs = interp_rec(exp, s_env, i_vs.sto)
            return ValStore(e_vs.val, ASto(loc, e_vs.val, e_vs.sto))

        match = Matcher([
            ('Num(n)', lambda n: ValStore(NumV(n), sto)),
            ('Add(l, r)', lambda l, r: do_sum(l, r, r_env, sto)),
            ('Sub(l, r)', lambda l, r: do_sub(l, r, r_env, sto)),
            ('Fun(iid, body)', lambda iid, body: ValStore(ClosureV(iid, body, r_env), sto)),
            ('IF0(c, t, f)', lambda c, t, f: do_if(c, t, f, r_env, sto)),
            ('Id(s)', lambda s: ValStore(sto_lookup(env_lookup(s, r_env), sto), sto)),
            ('App(f_exp, arg)', lambda f_exp, arg: do_app(f_exp, arg, r_env, sto)),
            ('Seqn(exp1, exp2)', lambda exp1, exp2: do_seq(exp1, exp2, r_env, sto)),
            ('Set(iid, exp)', lambda iid, exp: do_set(iid, exp, r_env, sto)),
        ])

        try:
            return match(r_expr)
        except:
            raise

    return interp_rec(expr, env, MTSto())


def run(prog):
    """
    run :: String -> String
    Evaluates a program as a string and results the resulting value as a string.
    If an error occurs, it will be displayed as a string.
    """
    try:
        res = interp(parse(prog), MTEnv())
        match = Matcher([
            ('NumV(n)', lambda n: n),
            ('ClosureV(_, _, _)', lambda: 'Function'),
        ])

        return match(res.val)

    except FreeIdentifier as e:
        print("error: identificador libre!! {}".format(e))
    except:
        print("error: expresión inválida {}".format(prog))
    exit(1)


if __name__ == "__main__":
    print(run("(8 10)"))
