# ================================================
# JAKSEL LANG - Extended Mini Interpreter (v2.3 FIXED)
# ================================================

import sys
import re

# ---------------------------
# LEXER
# ---------------------------
TOKEN_SPEC = [
    ('NUMBER',   r'\d+'),
    ('STRING',   r'"([^"\\]|\\.)*"'),
    ('NAME',     r'[A-Za-z_][A-Za-z0-9_]*'),
    ('OP',       r'==|!=|<=|>=|[+\-*/%<>]'),
    ('COLON',    r':'),
    ('LPAREN',   r'\('),
    ('RPAREN',   r'\)'),
    ('COMMA',    r','),
    ('NEWLINE',  r'\n'),
    ('SKIP',     r'[ \t]+'),
    ('MISMATCH', r'.'),
]

TOKEN_RE = re.compile('|'.join('(?P<%s>%s)' % pair for pair in TOKEN_SPEC))

class Token:
    def __init__(self, type_, value):
        self.type = type_
        self.value = value
    def __repr__(self):
        return f"Token({self.type},{self.value})"

def tokenize(code):
    tokens = []
    for mo in TOKEN_RE.finditer(code):
        kind = mo.lastgroup
        val = mo.group()
        if kind == 'NUMBER':
            tokens.append(Token('NUMBER', int(val)))
        elif kind == 'STRING':
            s = val[1:-1]
            s = s.replace('\\"', '"').replace('\\\\', '\\')
            tokens.append(Token('STRING', s))
        elif kind == 'NAME':
            tokens.append(Token('NAME', val))
        elif kind == 'OP':
            tokens.append(Token('OP', val))
        elif kind == 'COLON':
            tokens.append(Token('COLON', val))
        elif kind == 'LPAREN':
            tokens.append(Token('LPAREN', val))
        elif kind == 'RPAREN':
            tokens.append(Token('RPAREN', val))
        elif kind == 'COMMA':
            tokens.append(Token('COMMA', val))
        elif kind == 'NEWLINE':
            tokens.append(Token('NEWLINE', '\n'))
        elif kind == 'SKIP':
            continue
        elif kind == 'MISMATCH':
            raise SyntaxError(f'Unexpected char: {val}')
    tokens.append(Token('EOF', None))
    return tokens

# ---------------------------
# PARSER FIXED!
# ---------------------------
class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.i = 0
        self.cur = tokens[0]

    def advance(self):
        self.i += 1
        self.cur = self.tokens[self.i] if self.i < len(self.tokens) else Token('EOF', None)

    def expect(self, type_, value=None):
        if self.cur.type != type_:
            raise SyntaxError(f"Expected {type_}, got {self.cur}")
        if value and self.cur.value != value:
            raise SyntaxError(f"Expected {value}, got {self.cur.value}")
        self.advance()

    def parse(self):
        stmts = []
        while self.cur.type != 'EOF':
            if self.cur.type == 'NEWLINE':
                self.advance()
                continue
            stmts.append(self.statement())
        return ('PROGRAM', stmts)

    # -----------------------
    # STATEMENTS
    # -----------------------
    def statement(self):
        if self.cur.type == 'NAME':

            # LEGACY SYNTAX (add, multiply, subtract, divide)
            if self.cur.value in ('add', 'multiply', 'subtract', 'divide'):
                return self.legacy_stmt()

            kw = self.cur.value

            if kw == 'let': return self.let_stmt()
            if kw == 'say': return self.say_stmt()
            if kw == 'if': return self.if_stmt()
            if kw == 'repeat': return self.repeat_stmt()
            if kw == 'while': return self.while_stmt()
            if kw == 'function': return self.function_def()
            if kw == 'call': return self.call_stmt()
            if kw == 'return': return self.return_stmt()

            expr = self.expr()
            if self.cur.type == 'NEWLINE':
                self.advance()
            return ('EXPR_STMT', expr)

        expr = self.expr()
        if self.cur.type == 'NEWLINE':
            self.advance()
        return ('EXPR_STMT', expr)

    def legacy_stmt(self):
        opword = self.cur.value
        self.advance()

        left = self.expr()

        if self.cur.type != 'NAME':
            raise SyntaxError("Expected 'and/from/by'")
        connector = self.cur.value
        self.advance()

        right = self.expr()

        if opword == 'add': node = ('BINOP', '+', left, right)
        elif opword == 'multiply': node = ('BINOP', '*', left, right)
        elif opword == 'divide': node = ('BINOP', '/', left, right)
        elif opword == 'subtract':
            node = ('BINOP', '-', right, left) if connector == 'from' else ('BINOP', '-', left, right)

        if self.cur.type == 'NEWLINE':
            self.advance()

        return ('EXPR_STMT', node)

    def let_stmt(self):
        self.expect('NAME', 'let')
        if self.cur.type != 'NAME':
            raise SyntaxError("Expected variable name")
        name = self.cur.value
        self.advance()
        self.expect('NAME', 'be')
        expr = self.expr()
        if self.cur.type == 'NEWLINE':
            self.advance()
        return ('LET', name, expr)

    def say_stmt(self):
        self.expect('NAME', 'say')
        expr = self.expr()
        if self.cur.type == 'NEWLINE':
            self.advance()
        return ('SAY', expr)

    def if_stmt(self):
        self.expect('NAME', 'if')
        cond = self.expr()
        if self.cur.type == 'COLON':
            self.advance()
        then_block = self.block_until(['else', 'end'])

        else_block = []
        if self.cur.type == 'NAME' and self.cur.value == 'else':
            self.advance()
            if self.cur.type == 'COLON':
                self.advance()
            else_block = self.block_until(['end'])

        self.expect('NAME', 'end')

        return ('IF', cond, then_block, else_block)

    def repeat_stmt(self):
        self.expect('NAME', 'repeat')
        count = self.expr()
        if self.cur.type == 'NAME' and self.cur.value == 'times':
            self.advance()
        if self.cur.type == 'COLON':
            self.advance()
        block = self.block_until(['end'])
        self.expect('NAME', 'end')
        return ('REPEAT', count, block)

    def while_stmt(self):
        self.expect('NAME', 'while')
        cond = self.expr()
        if self.cur.type == 'COLON':
            self.advance()
        block = self.block_until(['end'])
        self.expect('NAME', 'end')
        return ('WHILE', cond, block)

    def function_def(self):
        self.expect('NAME', 'function')
        if self.cur.type != 'NAME':
            raise SyntaxError("Expected function name")
        fname = self.cur.value
        self.advance()

        params = []
        while self.cur.type == 'NAME':
            params.append(self.cur.value)
            self.advance()

        if self.cur.type == 'COLON':
            self.advance()

        block = self.block_until(['end'])
        self.expect('NAME', 'end')
        return ('FUNCTION', fname, params, block)

    def call_stmt(self):
        self.expect('NAME', 'call')
        fname = self.cur.value
        self.advance()

        args = []
        while self.cur.type not in ('NEWLINE', 'EOF'):
            args.append(self.expr())

        if self.cur.type == 'NEWLINE':
            self.advance()
        return ('CALL', fname, args)

    def return_stmt(self):
        self.expect('NAME', 'return')
        expr = self.expr()
        if self.cur.type == 'NEWLINE':
            self.advance()
        return ('RETURN', expr)

    def block_until(self, endwords):
        stmts = []
        while not (self.cur.type == 'NAME' and self.cur.value in endwords):
            if self.cur.type == 'EOF':
                raise SyntaxError("Unexpected EOF inside block")
            if self.cur.type == 'NEWLINE':
                self.advance()
                continue
            stmts.append(self.statement())
        return stmts

    # -----------------------
    # EXPRESSIONS
    # -----------------------
    def expr(self): return self._expr_rel()

    def _expr_rel(self):
        node = self._expr_add()
        while self.cur.type == 'OP' and self.cur.value in ('==','!=','>','<','>=','<='):
            op = self.cur.value
            self.advance()
            node = ('BINOP', op, node, self._expr_add())
        return node

    def _expr_add(self):
        node = self._expr_mul()
        while self.cur.type == 'OP' and self.cur.value in ('+','-'):
            op = self.cur.value
            self.advance()
            node = ('BINOP', op, node, self._expr_mul())
        return node

    def _expr_mul(self):
        node = self._expr_unary()
        while self.cur.type == 'OP' and self.cur.value in ('*','/', '%'):
            op = self.cur.value
            self.advance()
            node = ('BINOP', op, node, self._expr_unary())
        return node

    def _expr_unary(self):
        if self.cur.type == 'OP' and self.cur.value == '-':
            self.advance()
            return ('UNARY', '-', self._expr_primary())
        return self._expr_primary()

    def _expr_primary(self):
        tok = self.cur

        if tok.type == 'NUMBER':
            self.advance()
            return ('NUMBER', tok.value)

        if tok.type == 'STRING':
            self.advance()
            return ('STRING', tok.value)

        if tok.type == 'NAME':
            name = tok.value
            self.advance()

            # function call like f(x,y)
            if self.cur.type == 'LPAREN':
                self.advance()
                args = []
                if self.cur.type != 'RPAREN':
                    while True:
                        args.append(self.expr())
                        if self.cur.type == 'COMMA':
                            self.advance()
                            continue
                        break
                self.expect('RPAREN')
                return ('CALL_EXPR', name, args)

            return ('VAR', name)

        if tok.type == 'LPAREN':
            self.advance()
            node = self.expr()
            self.expect('RPAREN')
            return node

        raise SyntaxError(f"Unexpected token: {tok}")

# ---------------------------
# INTERPRETER
# ---------------------------
class ReturnException(Exception):
    def __init__(self, value):
        self.value = value

class Environment:
    def __init__(self, parent=None):
        self.vars = {}
        self.funcs = {}
        self.parent = parent

    def get(self, name):
        if name in self.vars: return self.vars[name]
        if self.parent: return self.parent.get(name)
        raise NameError(f"Undefined variable: {name}")

    def set(self, name, val):
        self.vars[name] = val

    def define_func(self, name, params, block):
        self.funcs[name] = (params, block)

    def get_func(self, name):
        if name in self.funcs: return self.funcs[name]
        if self.parent: return self.parent.get_func(name)
        return None

def call_builtin(name, args):
    if name == 'len': return len(args[0])
    if name == 'upper': return str(args[0]).upper()
    if name == 'lower': return str(args[0]).lower()
    if name == 'str': return str(args[0])
    if name == 'int': return int(args[0])
    raise NameError(f"Unknown builtin: {name}")

def eval_expr(node, env):
    t = node[0]

    if t == 'NUMBER': return node[1]
    if t == 'STRING': return node[1]
    if t == 'VAR': return env.get(node[1])
    if t == 'UNARY': return -eval_expr(node[2], env)

    if t == 'BINOP':
        op, a, b = node[1], node[2], node[3]
        A = eval_expr(a, env)
        B = eval_expr(b, env)
        return eval(f"A {op} B")

    if t == 'CALL_EXPR':
        fname = node[1]
        args = [eval_expr(a, env) for a in node[2]]

        if fname in ('len','upper','lower','str','int'):
            return call_builtin(fname, args)

        fn = env.get_func(fname)
        if not fn: raise NameError(f"Function not found: {fname}")
        params, block = fn

        newenv = Environment(parent=env)
        for p,a in zip(params,args):
            newenv.set(p,a)

        try:
            exec_block(block, newenv)
        except ReturnException as re:
            return re.value
        return None

    raise RuntimeError(f"Unknown expr node: {node}")

def exec_stmt(stmt, env):
    t = stmt[0]

    if t == 'LET':
        _, name, expr = stmt
        env.set(name, eval_expr(expr, env))

    elif t == 'SAY':
        _, expr = stmt
        print("[Jaksel vibes]", eval_expr(expr, env))

    elif t == 'IF':
        _, cond, then_block, else_block = stmt
        if eval_expr(cond, env): exec_block(then_block, env)
        else: exec_block(else_block, env)

    elif t == 'REPEAT':
        _, count, block = stmt
        for _ in range(int(eval_expr(count, env))):
            exec_block(block, env)

    elif t == 'WHILE':
        _, cond, block = stmt
        while eval_expr(cond, env):
            exec_block(block, env)

    elif t == 'FUNCTION':
        _, fname, params, block = stmt
        env.define_func(fname, params, block)

    elif t == 'CALL':
        _, fname, args_nodes = stmt
        args = [eval_expr(a, env) for a in args_nodes]

        if fname in ('len','upper','lower','str','int'):
            return call_builtin(fname, args)

        fn = env.get_func(fname)
        if not fn: raise NameError(f"Function not found: {fname}")
        params, block = fn

        newenv = Environment(parent=env)
        for p,a in zip(params,args):
            newenv.set(p,a)

        try:
            exec_block(block, newenv)
        except ReturnException as re:
            return re.value

    elif t == 'RETURN':
        raise ReturnException(eval_expr(stmt[1], env))

    elif t == 'EXPR_STMT':
        value = eval_expr(stmt[1], env)
        print("[Jaksel vibes]", value)

def exec_block(block, env):
    for s in block:
        exec_stmt(s, env)

# ---------------------------
# RUNNER / REPL FIXED
# ---------------------------
def run_source(src):
    tokens = tokenize(src)
    parser = Parser(tokens)
    ast = parser.parse()
    env = Environment()
    exec_block(ast[1], env)

def repl():
    print("=== JAKSEL LANG Extended Interpreter v2.3 ===")
    lines = []
    while True:
        line = input("> ")
        if line == "" and lines:
            code = "\n".join(lines) + "\n"
            try: run_source(code)
            except Exception as e: print("Error:", e)
            lines = []
            print("\n=== next program (blank line to run) ===")
        else:
            lines.append(line)

if __name__ == "__main__":
    repl()
